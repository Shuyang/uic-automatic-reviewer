package edu.uic.cs.automatic_reviewer.feature.term;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import edu.stanford.nlp.ling.TaggedWord;
import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentencePOSTagParser;
import edu.uic.cs.automatic_reviewer.feature.term.OpenNLPChunker.ChunkType;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

public class TechniqueTermsExtractor extends AbstractWordOperations {

	private static final Logger LOGGER = LogHelper
			.getLogger(TechniqueTermsExtractor.class);

	protected static final List<String> PARENTHESES_VALUES = Arrays.asList(
			"-LRB-", "-RRB-", "-LCB-", "-RCB-");

	private OpenNLPChunker chunker = new OpenNLPChunker();
	private SentencePOSTagParser sentencePOSTagParser = new SentencePOSTagParser();

	private static class NounPhrase extends ArrayList<TaggedWord> {
		private static final long serialVersionUID = 1L;

		private String contentSentence;

		public NounPhrase(List<TaggedWord> taggedWords, String contentSentence) {
			addAll(taggedWords);
			this.contentSentence = contentSentence;
		}

	}

	private List<NounPhrase> findAllNounPhrases(Paper paper) {

		List<List<TaggedWord>> sentences = sentencePOSTagParser
				.getSentencePOSTags(paper);

		List<NounPhrase> result = new ArrayList<NounPhrase>();
		for (List<TaggedWord> sentence : sentences) {
			List<List<TaggedWord>> nounPhrases = chunker.getChunks(sentence,
					ChunkType.NP);
			String sentenceString = createSentenceString(sentence);

			for (List<TaggedWord> nounPhrase : nounPhrases) {
				result.add(new NounPhrase(nounPhrase, sentenceString));
			}

		}

		return result;
	}

	private String createSentenceString(List<TaggedWord> sentence) {
		StringBuilder sb = new StringBuilder();
		for (TaggedWord taggedWord : sentence) {
			sb.append(taggedWord.word()).append(" ");
		}
		return sb.toString().trim();
	}

	public static class TermWithFrequency implements
			Comparable<TermWithFrequency> {
		private String term;
		private Integer frequency;

		private List<String> contentSentences;

		public TermWithFrequency(String term, Integer frequency,
				List<String> contentSentences) {
			this.term = term;
			this.frequency = frequency;
			this.contentSentences = contentSentences;
		}

		public String getTerm() {
			return term;
		}

		public Integer getFrequency() {
			return frequency;
		}

		public List<String> getContentSentences() {
			return contentSentences;
		}

		@Override
		public int compareTo(TermWithFrequency that) {
			return this.frequency.compareTo(that.frequency);
		}

		@Override
		public String toString() {
			return term + "=" + frequency;
		}
	}

	public List<TermWithFrequency> findAllPossibleTechniqueTerms(
			List<Paper> papers) {
		HashMap<String, Integer> countByTerms = new HashMap<String, Integer>();
		HashMap<String, List<String>> contentSentencesByTerms = new HashMap<String, List<String>>();

		for (Paper paper : papers) {

			if (LOGGER.isInfoEnabled()) {
				LOGGER.info(LogHelper.LOG_LAYER_TWO
						+ paper.getMetadata().getPaperFileName());
			}

			List<NounPhrase> nounPhrases = findAllNounPhrases(paper);
			// duplicate term in one paper only be counted once
			Map<String, String> termsForOnePaper = new HashMap<String, String>();

			for (NounPhrase nounPhrase : nounPhrases) {
				if (isPossibleTechniqueTerm(nounPhrase)) {
					String techniqueTerm = changeToStringTerm(nounPhrase);

					if (!termsForOnePaper.containsKey(techniqueTerm)
							|| termsForOnePaper.get(techniqueTerm).length() < nounPhrase.contentSentence
									.length())
						termsForOnePaper.put(techniqueTerm,
								nounPhrase.contentSentence);
				}
			}

			for (Entry<String, String> entry : termsForOnePaper.entrySet()) {
				String techniqueTerm = entry.getKey().trim();
				if (techniqueTerm.length() <= 1
						|| StringUtils.isAllLowerCase(techniqueTerm)) {
					continue;
				}

				// System.out.println(techniqueTerm);
				Integer count = countByTerms.get(techniqueTerm);
				countByTerms
						.put(techniqueTerm, (count == null) ? 1 : count + 1);

				String contentSentence = entry.getValue();
				List<String> contentSentences = contentSentencesByTerms
						.get(techniqueTerm);
				if (contentSentences == null) {
					contentSentences = new ArrayList<String>();
					contentSentencesByTerms
							.put(techniqueTerm, contentSentences);
				}
				contentSentences.add(contentSentence);
			}
		}

		List<TermWithFrequency> result = new ArrayList<TermWithFrequency>();
		for (Entry<String, Integer> entry : countByTerms.entrySet()) {
			result.add(new TermWithFrequency(entry.getKey(), entry.getValue(),
					contentSentencesByTerms.get(entry.getKey())));
		}
		Collections.sort(result, Collections.reverseOrder());

		return result;
	}

	private String changeToStringTerm(List<TaggedWord> nounPhrase) {
		StringBuilder sb = new StringBuilder();
		for (TaggedWord taggedWord : nounPhrase) {

			if (PARENTHESES_VALUES.contains(taggedWord.word())) {
				continue;
			}

			sb.append(taggedWord.word().replace("-", "")).append(" ");
		}

		return sb.toString().trim();
	}

	private boolean isPossibleTechniqueTerm(List<TaggedWord> nounPhrase) {
		boolean result = false;

		int index = 0;
		for (TaggedWord taggedWord : nounPhrase) {
			String term = taggedWord.word();
			String pos = taggedWord.tag();
			term = term.replace("-", "").trim();

			if (!(StringUtils.isAlpha(term) || pos.equals("POS"))
					|| "and".equals(term)) {
				// only accept terms or "'s"
				// "and" usually used between author names
				return false;
			}

			if (isStopWord(term)) {
				continue; // without increase index
			}

			if (index == 0) {
				// only one term, and in all upper case
				if (nounPhrase.size() == 1 && StringUtils.isAllUpperCase(term)
						&& term.length() > 1) {
					return true;
				}
			} else if (isCapitalized(term)) { // non-first term is capitalized
				result = true;
			}

			index++;
		}

		return result;
	}

	private boolean isCapitalized(String term) {
		return Character.isUpperCase(term.charAt(0));
	}

	public static void main(String[] args) throws IOException {
		TechniqueTermsExtractor extractor = new TechniqueTermsExtractor();
		List<Paper> papers = PaperCache.getInstance().getAllPapers();

		List<TermWithFrequency> termWithFrequencies = extractor
				.findAllPossibleTechniqueTerms(papers);
		System.out.println("=================================================");
		List<String> lines = new ArrayList<String>();
		TreeMap<Integer, Integer> countByFrequency = new TreeMap<Integer, Integer>(
				Collections.reverseOrder());

		for (TermWithFrequency termWithFrequency : termWithFrequencies) {
			System.out.println(termWithFrequency);
			lines.add(termWithFrequency.getFrequency() + "\t"
					+ termWithFrequency.getTerm() + "\t"
					+ termWithFrequency.getContentSentences());

			Integer count = countByFrequency.get(termWithFrequency
					.getFrequency());
			countByFrequency.put(termWithFrequency.getFrequency(),
					(count == null) ? 1 : count + 1);
		}

		System.out.println(countByFrequency);
		lines.add(countByFrequency.toString());

		FileUtils.writeLines(new File("possibleTechniqueTerms.txt"), lines);
	}

}
