package edu.uic.cs.automatic_reviewer.input.parse;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TransformerHandler;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.pdf.PDFParser;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.parser.PdfReaderContentParser;

import edu.uic.cs.automatic_reviewer.input.Metadata;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;
import edu.uic.cs.automatic_reviewer.input.Paper.Page;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

public class PaperParser /* extends AbstractWordOperations */{

	private static final Logger LOGGER = LogHelper.getLogger(PaperParser.class);

	private static final int PARAGRAPH_MIN_CHAR_LENGTH = 300;

	private static final Pattern REFERENCE_PATTERN = Pattern.compile(
			"(references|reference)\\s*", Pattern.CASE_INSENSITIVE);

	private static final String[] IGNORING_PARAGRAPH_PREFIXES = readLines("ignoring_paragraph_prefixes.txt");

	private PDFParser tikaParser = new PDFParser();
	private TransformerHandler handler;

	private AuthorInfoExtractor authorInfoExtractor = new AuthorInfoExtractorDBLP();

	@SuppressWarnings("unchecked")
	private static String[] readLines(String fileName) {
		InputStream input = PaperParser.class.getResourceAsStream(fileName);

		List<String> lines = null;
		try {
			lines = IOUtils.readLines(input);
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		} finally {
			IOUtils.closeQuietly(input);
		}

		return lines.toArray(new String[lines.size()]);
	}

	public PaperParser() {

		// SAX
		SAXTransformerFactory saxTransformerFactory = (SAXTransformerFactory) SAXTransformerFactory
				.newInstance();
		try {
			handler = saxTransformerFactory.newTransformerHandler();
		} catch (TransformerConfigurationException e) {
			throw new AutomaticReviewerException(e);
		}
		handler.getTransformer().setOutputProperty(OutputKeys.METHOD, "xml");
	}

	public Paper parse(File file) {

		Metadata metadata = new Metadata();
		DOMResult domResult = new DOMResult(); // use DOM

		// start parsing using tika
		parse(file, metadata, domResult);
		// end parsing

		Node document = domResult.getNode();
		try {
			Paper paper = parseDocument(document);
			// FIXME metadata

			return paper;
		} catch (Exception e) {
			throw new AutomaticReviewerException(e);
		}
	}

	public Paper parse(String filePath) {

		return parse(new File(filePath));
	}

	private Paper parseDocument(Node document) throws Exception {
		// (XPath doesn't work! I don't know why...)
		NodeList nodes = document.getChildNodes();
		Assert.isTrue(nodes.getLength() == 1);

		Node htmlNode = nodes.item(0);
		nodes = htmlNode.getChildNodes();
		Assert.isTrue(nodes.getLength() == 2);

		Node bodyNode = nodes.item(1);
		NodeList pageNodes = bodyNode.getChildNodes(); // page nodes

		Paper resultPaper = new Paper();

		List<String> rawParagraphs = new ArrayList<String>();
		for (int pageIndex = 0; pageIndex < pageNodes.getLength(); pageIndex++) {

			Node pageNode = pageNodes.item(pageIndex);
			Assert.isTrue("div".equalsIgnoreCase(pageNode.getLocalName()));
			Node classAttr = pageNode.getAttributes().getNamedItem("class");
			Assert.isTrue("page".equalsIgnoreCase(classAttr.getNodeValue()));

			Page page = new Page();

			NodeList paragraphNodes = pageNode.getChildNodes();
			for (int paragraphIndex = 0; paragraphIndex < paragraphNodes
					.getLength(); paragraphIndex++) {

				Element paragraphNode = (Element) paragraphNodes
						.item(paragraphIndex);
				Assert.isTrue("p".equalsIgnoreCase(paragraphNode.getLocalName()));

				// raw paragraph
				String rawParagraph = paragraphNode.getTextContent();

				page.addRawParagraph(rawParagraph);
				rawParagraphs.add(rawParagraph);
			}

			resultPaper.addPage(page);
		}

		parseRawParagraphs(rawParagraphs, resultPaper);

		return resultPaper;
	}

	private void parseRawParagraphs(List<String> rawParagraphs,
			Paper resultPaper) {

		boolean paperStarted = false; // if entire paper started
		boolean contentStarted = false; // if the content started
		boolean referencesStart = false;
		// part of the first reference content may in the same line of
		// "references", so we need to add this is this happens
		String firstReferencePrefix = null;

		List<String> rawAuthorInfos = new ArrayList<String>();

		for (int index = 0; index < rawParagraphs.size(); index++) {

			String tidiedRawParagraph = tidyParagraph(rawParagraphs.get(index));

			if (tidiedRawParagraph.length() == 0 /* && !paperStarted */
					|| StringUtils.startsWithAny(tidiedRawParagraph,
							IGNORING_PARAGRAPH_PREFIXES)) {
				continue;
			}

			if (!paperStarted) {

				// first paragraph is the title
				// tidiedRawParagraph = tidiedRawParagraph.split("\\r?\\n")[0];
				tidiedRawParagraph = tidiedRawParagraph.replaceAll("\\s+", " ");
				resultPaper.setTitle(tidiedRawParagraph);
				paperStarted = true;

				LOGGER.info("[TITLE]\t" + tidiedRawParagraph + "\n");

			} else if (!contentStarted) {

				if ("abstract".equalsIgnoreCase(tidiedRawParagraph)) {
					continue; // ignore paragraph contains only "abstract"
				} else if (tidiedRawParagraph.length() > PARAGRAPH_MIN_CHAR_LENGTH) {

					resultPaper.setAbstract(tidiedRawParagraph);
					contentStarted = true;

					LOGGER.info("[ABSTRACT]\n" + tidiedRawParagraph + "\n");

				} else // authors and university
				{
					rawAuthorInfos.add(tidiedRawParagraph);
				}

			} else { // content
				if (!referencesStart) // normal content
				{
					// references starts
					if (StringUtils.startsWithIgnoreCase(tidiedRawParagraph,
							"reference")) {

						Matcher matcher = REFERENCE_PATTERN
								.matcher(tidiedRawParagraph);
						String possibleReference = matcher.replaceFirst("");
						if (possibleReference.trim().length() > 0) {
							firstReferencePrefix = possibleReference;
						}

						referencesStart = true;
					} else {
						// normal content
						// TODO
					}
				} else // references
				{
					if (firstReferencePrefix != null) {
						tidiedRawParagraph = firstReferencePrefix + " "
								+ tidiedRawParagraph;
						firstReferencePrefix = null;
					}
					resultPaper.addReference(tidiedRawParagraph);

					LOGGER.info("[REFERENCE]\t" + tidiedRawParagraph);
				}
			}

		} // end rawParagraphs "for"

		authorInfoExtractor
				.parseAndFillAuthorInfos(resultPaper, rawAuthorInfos);

		System.out.println("=============================================");
		if (resultPaper.getAuthors() != null) {
			for (Author author : resultPaper.getAuthors()) {
				System.out.println(author);
			}
		}
	}

	private String tidyParagraph(String rawParagraph) {
		// TODO maybe do more jobs
		return rawParagraph.trim();
	}

	private void parse(File file, Metadata metadata, DOMResult domResult) {

		try {
			BufferedInputStream inputStream = new BufferedInputStream(
					new FileInputStream(file));

			// store the result inside a DOM structure
			handler.setResult(domResult);
			tikaParser
					.parse(inputStream, handler, metadata, new ParseContext());
		} catch (Exception e) {
			throw new AutomaticReviewerException(e);
		}
	}

	private int countImagesUsingIText(String filePath) {

		ImageCounter imageCounter = new ImageCounter();
		try {
			PdfReader iTextReader = new PdfReader(filePath);
			PdfReaderContentParser parser = new PdfReaderContentParser(
					iTextReader);
			for (int index = 1; index <= iTextReader.getNumberOfPages(); index++) {
				parser.processContent(index, imageCounter);
			}
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

		return imageCounter.getCount();
	}

	public static void main(String[] args) {

		File folder = new File(
				"D:\\Data\\UIC Documents\\Computer Science\\CS_521\\Project\\papers_acl_2012\\");

		PaperParser paperParser = new PaperParser();

		for (File file : folder.listFiles()) {
			if (file.isDirectory()) {
				continue;
			}
			paperParser.parse(file);
		}

	}
}
