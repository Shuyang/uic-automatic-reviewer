package edu.uic.cs.automatic_reviewer.prediction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.TreeMap;

import org.apache.log4j.Logger;

import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
import edu.stanford.nlp.trees.Tree;
import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceComplexity;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

public class SentenceComplexity4Prediction extends SentenceComplexity {

	private static final Logger LOGGER = Logger
			.getLogger(SentenceComplexity4Prediction.class);

	private static class WordOperations extends AbstractWordOperations {
		public List<String> splitIntoSentences(String input,
				boolean jointLastHyphenTerm) {
			return super.splitIntoSentences(input, jointLastHyphenTerm);
		}

		public LexicalizedParser getStanfordParser() {
			return super.getStanfordParser();
		}
	};

	private WordOperations wordOperations = new WordOperations();

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
	public TreeMap<Integer, Integer> measurePaperSentenceComplexity(
			final Paper paper) {

		if (LOGGER.isInfoEnabled()) {
			LOGGER.info(LogHelper.LOG_LAYER_ONE_BEGIN + "Parsing paper ["
					+ paper.getMetadata().getPaperFileName() + "]");
		}

		List<Tree> abstractSentences = parseAbstract(paper);
		List<List<Tree>> contentSentencesInParagraphs = parseContentSentences(paper);

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

		if (LOGGER.isInfoEnabled()) {
			LOGGER.info(LogHelper.LOG_LAYER_ONE_END + "Parsing paper ["
					+ paper.getMetadata().getPaperFileName() + "]\t"
					+ frequencyByComplexity);
		}

		return frequencyByComplexity;
	}

	private List<List<Tree>> parseContentSentences(Paper paper) {
		// content
		List<List<Tree>> contentSentenceTrees = new ArrayList<List<Tree>>(paper
				.getContentParagraphs().size());
		for (String paragraph : paper.getContentParagraphs()) {
			List<String> sentences = wordOperations.splitIntoSentences(
					paragraph, true);
			List<Tree> treesOfParagraph = new ArrayList<Tree>(sentences.size());
			for (String sentence : sentences) {
				if (sentence == null || sentence.trim().length() == 0) {
					continue;
				}
				Tree sentenceTree = parseSentence(sentence);
				treesOfParagraph.add(sentenceTree);
			}
			contentSentenceTrees.add(treesOfParagraph);
		}

		return contentSentenceTrees;
	}

	private List<Tree> parseAbstract(Paper paper) {

		// abstract
		if (paper.getAbstract() == null) {
			return null;
		}

		List<String> sentences = wordOperations.splitIntoSentences(
				paper.getAbstract(), true);
		List<Tree> abstractParseTrees = new ArrayList<Tree>(sentences.size());

		for (String sentence : sentences) {
			if (sentence == null || sentence.trim().length() == 0) {
				continue;
			}
			Tree sentenceTree = parseSentence(sentence);
			abstractParseTrees.add(sentenceTree);
		}

		return abstractParseTrees;
	}

	private Tree parseSentence(String sentence) {
		Tree tree = null;
		try {
			tree = wordOperations.getStanfordParser().apply(sentence);
			if (LOGGER.isInfoEnabled()) {
				LOGGER.info(tree);
			}
		} catch (Exception e) {
			LOGGER.error("Catch exception while parsing sentence [" + sentence
					+ "]", e);
		}

		return tree;
	}

	private void countComplexity(int complexity,
			TreeMap<Integer, Integer> frequencyByComplexity) {
		Integer frequency = frequencyByComplexity.get(complexity);
		frequencyByComplexity.put(complexity, (frequency == null) ? 1
				: (frequency + 1));
	}
}
