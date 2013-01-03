package edu.uic.cs.automatic_reviewer.common;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.analysis.CharArraySet;
import org.apache.lucene.analysis.PorterStemFilter;
import org.apache.lucene.analysis.PorterStemmerExporter;
import org.apache.lucene.analysis.StopFilter;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.standard.StandardFilter;
import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.lucene.util.Version;

import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public abstract class AbstractWordOperations {
	private PorterStemmerExporter porterStemmer = new PorterStemmerExporter();

	private static final String SENTENCE_MODEL_NAME = "opennlp_en-sent.bin";
	private static final SentenceModel SENTENCE_MODEL = loadSentenceModel();

	private volatile SentenceDetectorME sentenceDetector;

	private static volatile LexicalizedParser STANFORD_PARSER;

	private static SentenceModel loadSentenceModel() {
		InputStream inputStream = AbstractWordOperations.class
				.getResourceAsStream(SENTENCE_MODEL_NAME);
		try {
			return new SentenceModel(inputStream);
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		} finally {
			IOUtils.closeQuietly(inputStream);
		}
	}

	/**
	 * NOT been stemmed!
	 */
	public List<String> standardAnalyzeUsingDefaultStopWords(String rawString) {
		return standardAnalyze(rawString, StandardAnalyzer.STOP_WORDS_SET, true);
	}

	/**
	 * NOT been stemmed!
	 */
	public List<String> standardAnalyzeWithoutRemovingStopWords(
			String rawString, boolean lowercase) {
		return standardAnalyze(rawString, null, lowercase);
	}

	protected List<String> porterStemmingAnalyzeUsingDefaultStopWords(
			String rawString) {
		TokenStream tokenStream = constructStandardTokenStream(rawString,
				StandardAnalyzer.STOP_WORDS_SET, true);
		tokenStream = new PorterStemFilter(tokenStream);
		CharTermAttribute termAttribute = tokenStream
				.addAttribute(CharTermAttribute.class);

		List<String> result = new ArrayList<String>();
		try {
			while (tokenStream.incrementToken()) {
				// replace ',' for number like '3,000,230'
				result.add(termAttribute.toString().replace(",", ""));
			}
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

		return result;
	}

	public List<String> stem(List<String> termList) {
		List<String> result = new ArrayList<String>(termList.size());
		for (String term : termList) {
			result.add(stem(term));
		}

		return result;
	}

	protected String stem(String word) {
		return porterStemmer.stem(word);
	}

	/**
	 * NOT been stemmed!
	 * 
	 * StandardTokenizer -> StandardFilter -> LowerCaseFilter -> StopFilter
	 */
	private List<String> standardAnalyze(String rawString, Set<?> stopWords,
			boolean lowercase) {
		TokenStream tokenStream = constructStandardTokenStream(rawString,
				stopWords, lowercase);
		CharTermAttribute termAttribute = tokenStream
				.addAttribute(CharTermAttribute.class);

		List<String> result = new ArrayList<String>();
		try {
			while (tokenStream.incrementToken()) {
				// replace ',' for number like '3,000,230'
				result.add(termAttribute.toString().replace(",", ""));
			}
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

		return result;
	}

	private TokenStream constructStandardTokenStream(String rawString,
			Set<?> stopWords, boolean lowercase) {

		TokenStream tokenStream = null;
		if (lowercase) {
			// Use LUCENE_30 to support "'s"
			StandardAnalyzer standardAnalyzer = new StandardAnalyzer(
					Version.LUCENE_36, stopWords);
			tokenStream = standardAnalyzer.tokenStream(null, new StringReader(
					rawString));
		} else {
			tokenStream = createKeepingCasedTokenStream(new StringReader(
					rawString), stopWords);
		}

		return tokenStream;
	}

	private TokenStream createKeepingCasedTokenStream(final Reader reader,
			Set<?> stopWords) {
		stopWords = stopWords == null ? CharArraySet.EMPTY_SET : CharArraySet
				.unmodifiableSet(CharArraySet
						.copy(Version.LUCENE_36, stopWords));

		final StandardTokenizer src = new StandardTokenizer(Version.LUCENE_36,
				reader);
		src.setMaxTokenLength(StandardAnalyzer.DEFAULT_MAX_TOKEN_LENGTH);
		TokenStream tok = new StandardFilter(Version.LUCENE_36, src);
		// tok = new LowerCaseFilter(Version.LUCENE_36, tok);
		tok = new StopFilter(Version.LUCENE_36, tok, stopWords);

		return tok;
	}

	protected String trimStopWordsInBothSides(String rawString) {
		List<String> words = standardAnalyzeWithoutRemovingStopWords(rawString,
				true);

		int firstNonStopWordIndex = words.size();
		int lastNonStopWordIndex = -1;
		for (int index = 0; index < words.size(); index++) {
			String word = words.get(index);
			if (!isStopWord(word)) {
				firstNonStopWordIndex = index;
				break;
			}
		}

		for (int index = words.size() - 1; index >= 0; index--) {
			String word = words.get(index);
			if (!isStopWord(word)) {
				lastNonStopWordIndex = index;
				break;
			}
		}

		StringBuilder stringBuilder = new StringBuilder();
		if (lastNonStopWordIndex >= 0) {
			for (int index = firstNonStopWordIndex; index < lastNonStopWordIndex; index++) {
				stringBuilder.append(words.get(index));
				stringBuilder.append(' ');
			}
			stringBuilder.append(words.get(lastNonStopWordIndex));
		}

		return stringBuilder.toString();
	}

	protected boolean isStopWord(String word) {
		word = word.toLowerCase();
		return StandardAnalyzer.STOP_WORDS_SET.contains(word);
	}

	protected boolean containsAny(String target, Collection<String> terms,
			boolean ignoreCase) {
		for (String term : terms) {
			boolean contains = false;

			if (!ignoreCase) {
				contains = StringUtils.contains(target, term);
			} else {
				contains = StringUtils.containsIgnoreCase(target, term);
			}

			if (contains) {
				return true;
			}
		}

		return false;
	}

	protected List<String> splitIntoSentences(String input) {
		return splitIntoSentences(input, false);
	}

	/**
	 * 
	 * If jointLastHyphenTerm=true, the last term which contains hyphen will be
	 * connect with term in next line but keeping the hyphen. For example: <br>
	 * <b>"last frame of a seg-<br>
	 * ment." <br>
	 * </b> will become <br>
	 * <b>"last frame of a seg-ment."</b>
	 * 
	 * @param input
	 * @param jointLastHyphenTerm
	 * @return
	 */
	protected List<String> splitIntoSentences(String input,
			boolean jointLastHyphenTerm) {
		if (sentenceDetector == null) {
			synchronized (this) {
				if (sentenceDetector == null) {
					sentenceDetector = new SentenceDetectorME(SENTENCE_MODEL);
				}
			}
		}

		String[] sentences = sentenceDetector.sentDetect(input);
		if (jointLastHyphenTerm) {
			return jointLastHyphenTerm(sentences);
		}

		return Arrays.asList(sentences);
	}

	private List<String> jointLastHyphenTerm(String[] sentences) {
		if (sentences == null || sentences.length == 0) {
			return Collections.emptyList();
		}

		List<String> result = new ArrayList<String>(sentences.length);
		for (String sentence : sentences) {
			sentence = sentence.replaceAll("-(\\r\\n|\\r|\\n)", "-");
			result.add(sentence);
		}
		return result;
	}

	protected LexicalizedParser getStanfordParser() {
		if (STANFORD_PARSER == null) {
			synchronized (AbstractWordOperations.class) {
				if (STANFORD_PARSER == null) {
					STANFORD_PARSER = LexicalizedParser.loadModel();
				}
			}
		}
		return STANFORD_PARSER;
	}
}
