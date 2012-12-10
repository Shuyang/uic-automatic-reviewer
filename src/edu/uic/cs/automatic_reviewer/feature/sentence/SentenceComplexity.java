package edu.uic.cs.automatic_reviewer.feature.sentence;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.log4j.Logger;

import com.db4o.ObjectSet;

import edu.stanford.nlp.trees.Tree;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceAnalyzer.ParsedPaper;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;

public class SentenceComplexity {

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
				: frequency + 1);
	}

	private void cacheAllPaperComplexities() {
		if (analyzer == null) {
			analyzer = new SentenceAnalyzer();
		}

		ObjectSet<ParsedPaper> allStoredPapers = analyzer
				.retrieveAllParsedPapers();
		cachedComplexityByPaperName = new HashMap<String, TreeMap<Integer, Integer>>(
				allStoredPapers.size());

		for (ParsedPaper parsedPaper : allStoredPapers) {

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

	public static void main(String[] args) {

		SentenceComplexity sentenceComplexity = new SentenceComplexity();

		List<Paper> papers = PaperCache.getInstance().getAllPapers();
		System.out.println(papers.size() + " papers. ");
		for (Paper paper : papers) {

			TreeMap<Integer, Integer> complexity = sentenceComplexity
					.measurePaperSentenceComplexity(paper);
			System.out.println(paper.getMetadata().getPaperFileName() + " | "
					+ complexity);
		}
	}

}
