package edu.uic.cs.automatic_reviewer.input.parse;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.HttpClient;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.log4j.Logger;
import org.apache.tika.metadata.Metadata;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

public class AuthorInfoExtractorDBLP extends AuthorInfoExtractorMetadata {

	private static final Logger LOGGER = LogHelper
			.getLogger(AuthorInfoExtractorDBLP.class);

	private DocumentBuilder documentBuilder = null;
	private XPath xPath = null;

	public AuthorInfoExtractorDBLP() {
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		try {
			documentBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new AutomaticReviewerException(e);
		}

		XPathFactory factory = XPathFactory.newInstance();
		xPath = factory.newXPath();
	}

	@Override
	public void parseAndFillAuthorInfos(Paper paper,
			List<String> rawAuthorInfos, Metadata tikaMetadata) {

		// retrieve names, and re-fill title if we matched one record in DBLP
		List<String> authorNames = gatherAuthorNamesAndReFillTitle(paper);

		if (authorNames.isEmpty()) {
			super.parseAndFillAuthorInfos(paper, rawAuthorInfos, tikaMetadata);
			return;
		}

		List<Author> authors = gatherEmailAndOrganizationInfos(authorNames,
				rawAuthorInfos);

		paper.setAuthors(authors);

	}

	private List<String> gatherAuthorNamesAndReFillTitle(Paper paper) {

		List<String> authorNames = new ArrayList<String>();

		String possibleTitle = paper.getTitle();
		Assert.notEmpty(possibleTitle);

		String title = null;
		try {
			while (true) {

				int numberOfSpaces = StringUtils.countMatches(possibleTitle,
						" ");
				if (numberOfSpaces <= TITLE_MIN_TERMS_NUMBER - 1) {
					break;
				}

				String retrievedXmlString = retrievePaperInfoFromDBLP(possibleTitle);
				Document document = documentBuilder.parse(new InputSource(
						new StringReader(retrievedXmlString)));
				title = parseAuthorNames(document, authorNames);

				if (title != null) { // matched or match too many
					break;
				}

				// noting matched, try shorter title
				possibleTitle = shrinkPossibleTitle(possibleTitle);
				if (possibleTitle == null) { // nothing more can be shrunk
					break;
				}
			}
		} catch (Throwable e) {
			LOGGER.error("Caught error while retrieve paper info. from DBLP! ",
					e);
			// throw new AutomaticReviewerException(e);
		}

		if (title == null || title.equals("")) {
			// really nothing matched, or too many matched.
			// keep the original title
			LOGGER.warn("No paper found in DBLP for possible title ["
					+ possibleTitle
					+ "]. Author names will be retrieved from metadata. ");
		} else {
			paper.setTitle(title); // update the title
			Assert.notEmpty(authorNames);
			// paper.setAuthors(authorNames);
		}

		return authorNames;
	}

	private String shrinkPossibleTitle(String possibleTitle) {

		int endIndex = possibleTitle.lastIndexOf(" ");
		if (endIndex < 0) {
			return null;
		}

		String shrunkTitle = possibleTitle.substring(0, endIndex);
		if (shrunkTitle.equals(possibleTitle)) {
			return null;
		}

		return shrunkTitle;
	}

	// private String parseAuthorNames(Document document, List<String> names)
	// throws Exception {
	//
	// String matchedNumString = xPath.evaluate("/result/hits/@total",
	// document).trim();
	// if (matchedNumString.length() == 0) { // nothing matched
	// return null;
	// }
	//
	// int matchedNum = Integer.parseInt(matchedNumString);
	// if (matchedNum == 0) { // nothing matched
	// return null;
	// } else if (matchedNum > 1) { // should only one matched
	// return "";
	// }
	//
	// String titleNodeString = xPath.evaluate("//title", document);
	// System.out.println(titleNodeString);
	//
	// String titleNodeXmlString = "<title>" + titleNodeString + "</title>";
	//
	// Document titleDocument = documentBuilder.parse(new InputSource(
	// new StringReader(titleNodeXmlString)));
	// NodeList authorNodes = (NodeList) xPath.evaluate(
	// "/title/authors/author", titleDocument, XPathConstants.NODESET);
	//
	// Assert.isTrue(names.isEmpty());
	// for (int index = 0; index < authorNodes.getLength(); index++) {
	// String name = authorNodes.item(index).getFirstChild()
	// .getNodeValue();
	// names.add(name.trim());
	// }
	//
	// String title = xPath.evaluate("/title/title", titleDocument).trim();
	// if (title.endsWith(".")) {
	// title = title.substring(0, title.length() - 1);
	// }
	//
	// Assert.notEmpty(title);
	// Assert.notEmpty(names);
	// return title;
	// }

	private String parseAuthorNames(Document document, List<String> names)
			throws Exception {

		String matchedNumString = xPath.evaluate("/result/hits/@total",
				document).trim();
		if (matchedNumString.length() == 0) { // nothing matched
			return null;
		}

		int matchedNum = Integer.parseInt(matchedNumString);
		// nothing matched or too many paper matched
		if (matchedNum == 0 || matchedNum > MAX_NUMBER_OF_MATCHED_PAPER) {
			return null;
		}

		NodeList infoNodeList = (NodeList) xPath.evaluate("//hit/info",
				document, XPathConstants.NODESET);
		Assert.isTrue(infoNodeList.getLength() >= 1);

		// only retrieve the first matched one
		Node infoNode = infoNodeList.item(0);

		NodeList authorNodes = (NodeList) xPath.evaluate("authors/author",
				infoNode, XPathConstants.NODESET);

		Assert.isTrue(names.isEmpty());
		for (int index = 0; index < authorNodes.getLength(); index++) {
			String name = authorNodes.item(index).getFirstChild()
					.getNodeValue();
			names.add(name.trim());
		}

		String title = xPath.evaluate("//title", infoNode).trim();
		if (title.endsWith(".")) {
			title = title.substring(0, title.length() - 1);
		}

		Assert.notEmpty(title);
		Assert.notEmpty(names);
		return title;
	}

	private String retrievePaperInfoFromDBLP(String title) {

		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(LogHelper.LOG_LAYER_ONE_BEGIN
					+ "retrieve info from DBLP for title [" + title + "]");
		}

		title = title.replace("-", " ").replace(" ", "*%20").replace(":", "")
				.replace("`", "").toLowerCase(Locale.US);
		title = title + "*"; // add the last "*"
		String queryUrl = "http://www.dblp.org/search/api/?q=" + title
				+ "&format=xml";

		HttpClient httpclient = new DefaultHttpClient();
		try {
			HttpGet httpget = new HttpGet(queryUrl);

			// Create a response handler
			ResponseHandler<String> responseHandler = new BasicResponseHandler();
			try {
				String responseBody = httpclient.execute(httpget,
						responseHandler);

				if (LOGGER.isDebugEnabled()) {
					LOGGER.debug(LogHelper.LOG_LAYER_ONE_END
							+ "retrieved info from DBLP, response ["
							+ responseBody + "]");
				}

				return responseBody;
			} catch (IOException e) {
				throw new AutomaticReviewerException(e);
			}
		} finally {
			httpclient.getConnectionManager().shutdown();
		}

	}

}
