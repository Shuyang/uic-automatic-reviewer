package edu.uic.cs.automatic_reviewer.feature.sentence;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;

import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.trees.Tree;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceAnalyzer.ParsedPaper;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;

public class SentencePOSTagParser {

	private static final Logger LOGGER = LogHelper
			.getLogger(SentencePOSTagParser.class);

	private static final String SENTENCE_POS_TAG_CACHE_FILE = "data/paper_sentence_pos.cache";

	private SentenceAnalyzer analyzer = null;

	private volatile Map<String, List<List<TaggedWord>>> cachedSentencePOSTagsByPaperName;

	@SuppressWarnings("unchecked")
	public List<List<TaggedWord>> getSentencePOSTags(Paper paper) {

		if (cachedSentencePOSTagsByPaperName == null) {
			synchronized (this) {
				if (cachedSentencePOSTagsByPaperName == null) {
					LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
							+ "Deserializing from file ["
							+ SENTENCE_POS_TAG_CACHE_FILE + "]...");

					cachedSentencePOSTagsByPaperName = (Map<String, List<List<TaggedWord>>>) SerializationHelper
							.deserialize(SENTENCE_POS_TAG_CACHE_FILE);

					if (cachedSentencePOSTagsByPaperName == null) {
						LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
								+ "No cache file ["
								+ SENTENCE_POS_TAG_CACHE_FILE + "].");

						LOGGER.warn(LogHelper.LOG_LAYER_TWO_BEGIN
								+ " Cache all sentence POS tags... ");
						cacheAllSentencesWithPOSTags();
						LOGGER.warn(LogHelper.LOG_LAYER_TWO_END
								+ " Cache all sentence POS tags... done.");

					} else {
						LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
								+ "Deserializing from file ["
								+ SENTENCE_POS_TAG_CACHE_FILE + "]... done.");
					}
				}
			}
		}

		Assert.notNull(cachedSentencePOSTagsByPaperName);

		List<List<TaggedWord>> result = cachedSentencePOSTagsByPaperName
				.get(paper.getMetadata().getPaperFileName());
		return (result != null) ? result : Collections
				.<List<TaggedWord>> emptyList();
	}

	private void cacheAllSentencesWithPOSTags() {
		if (analyzer == null) {
			analyzer = new SentenceAnalyzer();
		}

		List<ParsedPaper> allStoredPapers = analyzer.retrieveAllParsedPapers();
		cachedSentencePOSTagsByPaperName = new HashMap<String, List<List<TaggedWord>>>(
				allStoredPapers.size());

		for (ParsedPaper parsedPaper : allStoredPapers) {

			List<List<TaggedWord>> sentencePOSTags = getherSentencePOSTags(parsedPaper);

			cachedSentencePOSTagsByPaperName.put(
					parsedPaper.getPaperFileName(), sentencePOSTags);
			LOGGER.warn(sentencePOSTags.size() + " sentences for ["
					+ parsedPaper.getPaperFileName() + "]");

		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Caching all paper sentence POS tags...");

		SerializationHelper.serialize(cachedSentencePOSTagsByPaperName,
				SENTENCE_POS_TAG_CACHE_FILE);

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
				+ "Caching all paper sentence POS tags... Done.");
	}

	private List<List<TaggedWord>> getherSentencePOSTags(ParsedPaper parsedPaper) {

		List<Tree> abstractSentences = parsedPaper.getAbstractParseTrees();
		List<List<Tree>> contentSentencesInParagraphs = parsedPaper
				.getContentSentenceTrees();

		List<List<TaggedWord>> sentencePOSTags = new ArrayList<List<TaggedWord>>();

		if (abstractSentences != null) {
			for (Tree sentence : abstractSentences) {
				ArrayList<TaggedWord> taggedWords = sentence.taggedYield();
				sentencePOSTags.add(taggedWords);
			}
		}

		if (contentSentencesInParagraphs != null) {
			for (List<Tree> contentSentences : contentSentencesInParagraphs) {
				for (Tree sentence : contentSentences) {
					ArrayList<TaggedWord> taggedWords = sentence.taggedYield();
					sentencePOSTags.add(taggedWords);
				}
			}
		}
		return sentencePOSTags;
	}

	@SuppressWarnings("unused")
	private void addNewSentencePOSTagsToCache(int year) {
		List<Paper> papers = PaperCache.getInstance().getPapers(year);
		// check
		for (Paper paper : papers) {
			List<List<TaggedWord>> posTags = getSentencePOSTags(paper);
			Assert.isTrue(posTags.isEmpty());
		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Adding new sentence POS tags for papers in year [" + year
				+ "]...");

		if (analyzer == null) {
			analyzer = new SentenceAnalyzer();
		}

		for (ParsedPaper parsedPaper : analyzer.retrieveParsedPapers(year)) {
			List<List<TaggedWord>> sentencePOSTags = getherSentencePOSTags(parsedPaper);

			cachedSentencePOSTagsByPaperName.put(
					parsedPaper.getPaperFileName(), sentencePOSTags);
			LOGGER.warn(sentencePOSTags.size() + " sentences for ["
					+ parsedPaper.getPaperFileName() + "]");
		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
				+ "Adding new sentence POS tags for papers in year [" + year
				+ "]... Done.");

		// remove old one
		FileUtils.deleteQuietly(new File(SENTENCE_POS_TAG_CACHE_FILE));
		System.err.println("Cache file[" + SENTENCE_POS_TAG_CACHE_FILE
				+ "] has been removed.");

		// re-cache the new one
		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Caching sentence POS tags...");
		SerializationHelper.serialize(cachedSentencePOSTagsByPaperName,
				SENTENCE_POS_TAG_CACHE_FILE);
		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
				+ "Caching all sentence POS tags... Done.");
	}

	public static void main(String[] args) {

		SentencePOSTagParser posTagParser = new SentencePOSTagParser();

		List<Paper> papers = PaperCache.getInstance().getAllPapers();
		System.out.println(papers.size() + " papers. ");
		for (Paper paper : papers) {

			List<List<TaggedWord>> sentences = posTagParser
					.getSentencePOSTags(paper);
			System.out.println(paper.getMetadata().getPaperFileName() + " | "
					+ sentences.size());
		}
	}
}
