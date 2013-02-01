package edu.uic.cs.automatic_reviewer.feature.sentence;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;

import edu.stanford.nlp.trees.Tree;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceAnalyzer.ParsedPaper;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;

public class SentenceComplexity implements Constants.SentenceComplexity,
		Feature {

	private static final Logger LOGGER = Logger
			.getLogger(SentenceComplexity.class);

	private static final String COMPLEXITY_CACHE_FILE = "data/paper_complexity.cache";

	private SentenceAnalyzer analyzer = null;

	private volatile Map<String, TreeMap<Integer, Integer>> cachedComplexityByPaperName;

	private int measureSentence(Tree sentence) {
		if (sentence != null) {
			return sentence.depth();
		}

		return 0;
	}

	/**
	 * Get sentence complexities with their frequency <br>
	 * TreeMap&lt;Complexity, Frequency&gt;<br>
	 * Ordered by descending order of complexity
	 * 
	 * @param paper
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public TreeMap<Integer, Integer> measurePaperSentenceComplexity(
			final Paper paper) {

		if (cachedComplexityByPaperName == null) {
			synchronized (this) {
				if (cachedComplexityByPaperName == null) {
					cachedComplexityByPaperName = (Map<String, TreeMap<Integer, Integer>>) SerializationHelper
							.deserialize(COMPLEXITY_CACHE_FILE);
					if (cachedComplexityByPaperName == null) {
						cacheAllPaperComplexities();
					}
				}
			}
		}

		Assert.notNull(cachedComplexityByPaperName);

		TreeMap<Integer, Integer> result = cachedComplexityByPaperName
				.get(paper.getMetadata().getPaperFileName());
		return (result != null) ? result : new TreeMap<Integer, Integer>();
	}

	private void countComplexity(int complexity,
			TreeMap<Integer, Integer> frequencyByComplexity) {
		Integer frequency = frequencyByComplexity.get(complexity);
		frequencyByComplexity.put(complexity, (frequency == null) ? 1
				: (frequency + 1));
	}

	private void cacheAllPaperComplexities() {
		if (analyzer == null) {
			analyzer = new SentenceAnalyzer();
		}

		List<ParsedPaper> allStoredPapers = analyzer.retrieveAllParsedPapers();
		cachedComplexityByPaperName = new HashMap<String, TreeMap<Integer, Integer>>(
				allStoredPapers.size());

		for (ParsedPaper parsedPaper : allStoredPapers) {

			TreeMap<Integer, Integer> frequencyByComplexity = measureFrequencyByComplexity(parsedPaper);

			cachedComplexityByPaperName.put(parsedPaper.getPaperFileName(),
					frequencyByComplexity);
			LOGGER.warn("Complexity for [" + parsedPaper.getPaperFileName()
					+ "]\t" + frequencyByComplexity);
		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Caching all paper complexities...");

		SerializationHelper.serialize(cachedComplexityByPaperName,
				COMPLEXITY_CACHE_FILE);

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
				+ "Caching all paper complexities... Done.");
	}

	private TreeMap<Integer, Integer> measureFrequencyByComplexity(
			ParsedPaper parsedPaper) {

		List<Tree> abstractSentences = parsedPaper.getAbstractParseTrees();
		List<List<Tree>> contentSentencesInParagraphs = parsedPaper
				.getContentSentenceTrees();

		// reverse order, most complex first
		TreeMap<Integer, Integer> frequencyByComplexity = new TreeMap<Integer, Integer>(
				Collections.reverseOrder());

		if (abstractSentences != null) {
			for (Tree sentence : abstractSentences) {
				int complexity = measureSentence(sentence);
				countComplexity(complexity, frequencyByComplexity);
			}
		}

		if (contentSentencesInParagraphs != null) {
			for (List<Tree> contentSentences : contentSentencesInParagraphs) {
				for (Tree sentence : contentSentences) {
					int complexity = measureSentence(sentence);
					countComplexity(complexity, frequencyByComplexity);
				}
			}
		}
		return frequencyByComplexity;
	}

	@SuppressWarnings("unused")
	private void addNewSentenceComplexitiesToCache(int year) {
		List<Paper> papers = PaperCache.getInstance().getPapers(year);
		// check
		for (Paper paper : papers) {
			TreeMap<Integer, Integer> complexities = measurePaperSentenceComplexity(paper);
			Assert.isTrue(complexities.isEmpty());
		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Adding new sentence complexities for papers in year ["
				+ year + "]...");

		if (analyzer == null) {
			analyzer = new SentenceAnalyzer();
		}

		for (ParsedPaper parsedPaper : analyzer.retrieveParsedPapers(year)) {
			TreeMap<Integer, Integer> frequencyByComplexity = measureFrequencyByComplexity(parsedPaper);

			cachedComplexityByPaperName.put(parsedPaper.getPaperFileName(),
					frequencyByComplexity);
			LOGGER.warn("Complexity for [" + parsedPaper.getPaperFileName()
					+ "]\t" + frequencyByComplexity);
		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
				+ "Adding new sentence complexities for papers in year ["
				+ year + "]... Done.");

		// remove old one
		FileUtils.deleteQuietly(new File(COMPLEXITY_CACHE_FILE));
		System.err.println("Cache file[" + COMPLEXITY_CACHE_FILE
				+ "] has been removed.");

		// re-cache the new one
		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Caching all paper complexities...");
		SerializationHelper.serialize(cachedComplexityByPaperName,
				COMPLEXITY_CACHE_FILE);
		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END
				+ "Caching all paper complexities... Done.");
	}

	public static void main(String[] args) {

		SentenceComplexity sentenceComplexity = new SentenceComplexity();

		List<Paper> papers = PaperCache.getInstance().getAllPapers();
		System.out.println(papers.size() + " papers. ");

		int num = sentenceComplexity.getNumberOfSubFeatures();
		for (Paper paper : papers) {
			double[] complexity = sentenceComplexity.getInstanceValues(paper);
			Assert.isTrue(num == complexity.length);

			System.out.println(paper.getMetadata().getPaperFileName() + "\t"
					+ Arrays.toString(complexity));
		}
	}

	@Override
	public double[] getInstanceValues(Paper paper) {
		TreeMap<Integer, Integer> sentenceNumberByComplexity = measurePaperSentenceComplexity(paper);
		double[] result = new double[MAX_COMPLEXITY];

		int totalSentenceNumber = 0; // count total number
		for (Integer number : sentenceNumberByComplexity.values()) {
			totalSentenceNumber += number;
		}
		if (totalSentenceNumber == 0) {
			LOGGER.warn("NO sentences retrieved for paper ["
					+ paper.getMetadata().getPaperFileName() + "]");
			Arrays.fill(result, Double.NaN);
			return result;
		}

		for (Entry<Integer, Integer> frequencyByComplexity : sentenceNumberByComplexity
				.entrySet()) {
			int complexity = frequencyByComplexity.getKey();
			double frequency = frequencyByComplexity.getValue().doubleValue();

			if (complexity < MAX_COMPLEXITY) {
				result[complexity] += (frequency / totalSentenceNumber);
			} else {
				LOGGER.warn("Paper [" + paper.getMetadata().getPaperFileName()
						+ "] has sentence with complexity >= " + MAX_COMPLEXITY);
				result[result.length - 1] += (frequency / totalSentenceNumber);
			}

		}

		return result;
	}

	@Override
	public int getNumberOfSubFeatures() {
		return MAX_COMPLEXITY;
	}

	@Override
	public String[] getSubFeatureNames() {

		String[] result = new String[MAX_COMPLEXITY];
		for (int index = 0; index < MAX_COMPLEXITY; index++) {
			result[index] = getName() + "_" + index;
		}

		return result;
	}

	@Override
	public String getName() {
		return "SEN_COMPX";
	}

}
