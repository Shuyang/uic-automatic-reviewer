package edu.uic.cs.automatic_reviewer.feature.sentence;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Formatter;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.log4j.Logger;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

import edu.stanford.nlp.trees.Tree;
import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

class SentenceAnalyzer {

	private static final Logger LOGGER = LogHelper
			.getLogger(SentenceAnalyzer.class);

	private static final String DB4OFILENAME = "object_db/paper_parse_tree.db4o";
	private static final ObjectContainer DB = Db4oEmbedded
			.openFile(DB4OFILENAME);

	private static final int DOCUMENT_PARSING_THREAD_POOL_SIZE = Runtime
			.getRuntime().availableProcessors() + 1;

	static class ParsedPaper {

		private String paperFileName = null;

		private List<Tree> abstractParseTrees = null;
		private List<List<Tree>> contentSentenceTrees = null;

		private byte[] serializedAbstractParseTrees;
		private byte[] serializedContentSentenceTrees;

		public ParsedPaper(String paperFileName) {
			this.paperFileName = paperFileName;
		}

		public void setAbstractParseTrees(List<Tree> abstractParseTrees) {
			serializedAbstractParseTrees = SerializationUtils
					.serialize((Serializable) abstractParseTrees);
		}

		public void setContentSentenceTrees(
				List<List<Tree>> contentSentenceTrees) {
			serializedContentSentenceTrees = SerializationUtils
					.serialize((Serializable) contentSentenceTrees);
		}

		public String getPaperFileName() {
			return paperFileName;
		}

		@SuppressWarnings("unchecked")
		public synchronized List<Tree> getAbstractParseTrees() {
			if (abstractParseTrees == null) {
				abstractParseTrees = (serializedAbstractParseTrees != null) ? (List<Tree>) SerializationUtils
						.deserialize(serializedAbstractParseTrees)
						: Collections.<Tree> emptyList();
			}
			return abstractParseTrees;
		}

		@SuppressWarnings("unchecked")
		public synchronized List<List<Tree>> getContentSentenceTrees() {
			if (contentSentenceTrees == null) {
				contentSentenceTrees = (serializedContentSentenceTrees != null) ? (List<List<Tree>>) SerializationUtils
						.deserialize(serializedContentSentenceTrees)
						: Collections.<List<Tree>> emptyList();
			}
			return contentSentenceTrees;
		}
	}

	private ParsedPaper cachedPaper = null;

	public List<List<Tree>> getContentParseTrees(Paper paper) {
		if (cachedPaper != null
				&& cachedPaper.getPaperFileName().equals(
						paper.getMetadata().getPaperFileName())) {

			// System.out.println("using cache in getContentParseTrees()");
			return cachedPaper.getContentSentenceTrees();
		}

		ParsedPaper parsedPaperInDB = retrieveParsedPaperFromDB(paper);
		if (parsedPaperInDB == null) {
			return null;
		}

		cachedPaper = parsedPaperInDB;

		return parsedPaperInDB.getContentSentenceTrees();
	}

	ObjectSet<ParsedPaper> retrieveAllParsedPapers() {
		ObjectSet<ParsedPaper> result = DB.query(ParsedPaper.class);
		return result;
	}

	public List<Tree> getAbstractParseTrees(Paper paper) {
		if (cachedPaper != null
				&& cachedPaper.getPaperFileName().equals(
						paper.getMetadata().getPaperFileName())) {
			// System.out.println("using cache in getAbstractParseTrees()");
			return cachedPaper.getAbstractParseTrees();
		}

		ParsedPaper parsedPaperInDB = retrieveParsedPaperFromDB(paper);
		if (parsedPaperInDB == null) {
			return null;
		}

		cachedPaper = parsedPaperInDB;

		return parsedPaperInDB.getAbstractParseTrees();
	}

	private ParsedPaper retrieveParsedPaperFromDB(Paper paper) {
		final String paperFileName = paper.getMetadata().getPaperFileName();

		// ObjectSet<ParsedPaper> result = DB.queryByExample(new ParsedPaper(
		// paperFileName));
		ObjectSet<ParsedPaper> result = DB.query(new Predicate<ParsedPaper>() {
			private static final long serialVersionUID = 1L;

			@Override
			public boolean match(ParsedPaper candidate) {
				return paperFileName.equals(candidate.getPaperFileName());
			}
		});

		if (result == null || result.isEmpty()) {
			LOGGER.warn("No parse-tree stored in Database[" + DB4OFILENAME
					+ "] for Paper[" + paperFileName
					+ "]. Check if you have indexed all the papers.");
			return null;
		}

		Assert.isTrue(result.size() == 1, "Duplicate data for papaer["
				+ paperFileName + "]");
		return result.get(0);

	}

	private void parseAndStoreAllPapers(List<Paper> papers) {

		Assert.isTrue(false,
				"Comment this line if you REALLY want to parse and store papers. ");

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN + "Parse [" + papers.size()
				+ "] papers...");
		long begin = System.currentTimeMillis();

		parseAndStoreAllPapersInternal(papers);

		double end = System.currentTimeMillis();
		Formatter formatter = new Formatter();
		formatter.format("%.2f", (end - begin) / 1000 / 60 / 60); // s/min/hour
		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END + "All [" + papers.size()
				+ "] " + "papers have been parsed in " + formatter.toString()
				+ " hours.");
	}

	private static class ParseTreeTask extends AbstractWordOperations implements
			Runnable {

		private Paper paper;

		private ParseTreeTask(Paper paper) {
			Assert.notNull(paper);
			this.paper = paper;
		}

		@Override
		public void run() {
			String paperFileName = paper.getMetadata().getPaperFileName();
			try {
				ParsedPaper result = parsePaper(paper, paperFileName);
				// store into DB
				storePaper(result, paperFileName);
			} catch (Throwable e) {
				LOGGER.error("Catch exception when parsing paper ["
						+ paperFileName + "]", e);
			}
		}

		private void storePaper(ParsedPaper paper, String paperFileName) {

			DB.store(paper);
			DB.commit();

			LOGGER.warn("Added paper [" + paperFileName + "] into DB");
		}

		private ParsedPaper parsePaper(Paper paper, String paperFileName) {

			String threadName = Thread.currentThread().getName();
			LOGGER.warn(LogHelper.LOG_LAYER_TWO_BEGIN + "Thread[" + threadName
					+ "] begin to process paper[" + paperFileName + "]");

			ParsedPaper result = new ParsedPaper(paperFileName);

			// abstract
			if (paper.getAbstract() != null) {
				List<String> sentences = splitIntoSentences(
						paper.getAbstract(), true);
				List<Tree> abstractParseTrees = new ArrayList<Tree>(
						sentences.size());

				for (String sentence : sentences) {
					if (sentence == null || sentence.trim().length() == 0) {
						continue;
					}
					Tree sentenceTree = parseSentence(sentence, threadName);
					abstractParseTrees.add(sentenceTree);
				}

				result.setAbstractParseTrees(abstractParseTrees);
			}

			// content
			List<List<Tree>> contentSentenceTrees = new ArrayList<List<Tree>>(
					paper.getContentParagraphs().size());
			for (String paragraph : paper.getContentParagraphs()) {
				List<String> sentences = splitIntoSentences(paragraph, true);
				List<Tree> treesOfParagraph = new ArrayList<Tree>(
						sentences.size());
				for (String sentence : sentences) {
					if (sentence == null || sentence.trim().length() == 0) {
						continue;
					}
					Tree sentenceTree = parseSentence(sentence, threadName);
					treesOfParagraph.add(sentenceTree);
				}
				contentSentenceTrees.add(treesOfParagraph);
			}
			result.setContentSentenceTrees(contentSentenceTrees);

			LOGGER.warn(LogHelper.LOG_LAYER_TWO_END + "Thread[" + threadName
					+ "] finished processing paper[" + paperFileName + "]");

			return result;
		}

		private Tree parseSentence(String sentence, String treadName) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("Thread[" + treadName + "] Parsing [" + sentence
						+ "]");
			}
			Tree tree = null;
			try {
				tree = getStanfordParser().apply(sentence);
			} catch (Exception e) {
				LOGGER.error("Catch exception while parsing sentence ["
						+ sentence + "]", e);
			}
			LOGGER.warn("Thread[" + treadName + "] Parsed tree [" + tree + "]");

			return tree;
		}
	}

	private void parseAndStoreAllPapersInternal(List<Paper> papers) {

		ExecutorService threadPool = Executors
				.newFixedThreadPool(DOCUMENT_PARSING_THREAD_POOL_SIZE);

		List<Callable<Object>> todo = new ArrayList<Callable<Object>>(
				papers.size());
		for (Paper paper : papers) {
			todo.add(Executors.callable(new ParseTreeTask(paper)));
		}

		try {
			threadPool.invokeAll(todo);
		} catch (Throwable e) {
			LOGGER.error("Catch exception", e);
			throw new AutomaticReviewerException(e);
		} finally {
			threadPool.shutdown();
		}
	}

	public static void main(String[] args) throws Exception {

		List<Paper> papers = PaperCache.getInstance().getAllPapers();
		new SentenceAnalyzer().parseAndStoreAllPapers(papers);
		System.out.println(papers.size());

		// SentenceAnalyzer analyzer = new SentenceAnalyzer();
		// for (Paper paper : papers) {
		// List<List<Tree>> contents = analyzer.getContentParseTrees(paper);
		// for (List<Tree> trees : contents) {
		// for (Tree tree : trees) {
		// System.out.println(tree);
		// }
		// System.out.println("----------------------");
		// }
		//
		// Thread.sleep(2000);
		// }
	}
}
