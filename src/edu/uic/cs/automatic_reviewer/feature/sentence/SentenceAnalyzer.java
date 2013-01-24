package edu.uic.cs.automatic_reviewer.feature.sentence;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Formatter;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
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

	private static final String _DB4OFILENAME = "object_db/paper_parse_tree_$YEAR$.db4o";
	private static final Map<Integer, ObjectContainer> DB_BY_YEAR = openObjectDBs(
			2007, 2010, 2011, 2012);

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

	private static Map<Integer, ObjectContainer> openObjectDBs(int... years) {

		Map<Integer, ObjectContainer> result = new TreeMap<Integer, ObjectContainer>();

		for (Integer year : years) {
			String dbName = _DB4OFILENAME.replace("$YEAR$", year.toString());
			ObjectContainer db = Db4oEmbedded.openFile(dbName);
			result.put(year, db);
		}

		return result;
	}

	List<ParsedPaper> retrieveAllParsedPapers() {
		List<ParsedPaper> result = new ArrayList<ParsedPaper>();
		for (ObjectContainer db : DB_BY_YEAR.values()) {
			result.addAll(db.query(ParsedPaper.class));
		}
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
		Predicate<ParsedPaper> queryFilterCondition = new Predicate<ParsedPaper>() {
			private static final long serialVersionUID = 1L;

			@Override
			public boolean match(ParsedPaper candidate) {
				return paperFileName.equals(candidate.getPaperFileName());
			}
		};

		ObjectSet<ParsedPaper> result = null;
		for (ObjectContainer db : DB_BY_YEAR.values()) {
			result = db.query(queryFilterCondition);
			if (result != null && !result.isEmpty()) {
				break;
			}
		}

		if (result == null || result.isEmpty()) {
			LOGGER.warn("No parse-tree stored in Object-Database for Paper["
					+ paperFileName
					+ "]. Check if you have indexed all the papers.");
			return null;
		}

		Assert.isTrue(result.size() == 1, "Duplicate data for papaer["
				+ paperFileName + "]");
		return result.get(0);

	}

	private void parseAndStoreAllPapers(int year) {

		List<Paper> papers = PaperCache.getInstance().getPapers(year);

		// Assert.isTrue(false,
		// "Comment this line if you REALLY want to parse and store papers. ");

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN + "Parse [" + papers.size()
				+ "] papers...");
		long begin = System.currentTimeMillis();

		parseAndStoreAllPapersInternal(papers, year);

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
		private ObjectContainer db;

		private ParseTreeTask(Paper paper, ObjectContainer db) {
			Assert.notNull(paper);
			Assert.notNull(db);
			this.paper = paper;
			this.db = db;
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

			db.store(paper);
			db.commit();

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
			// TODO detect the sentence is a table, and ignore it

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

	private void parseAndStoreAllPapersInternal(List<Paper> papers, int year) {

		// get papers in cache now
		List<ParsedPaper> cachedPapers = retrieveAllParsedPapers();
		Set<String> cachedPaperFileNames = new HashSet<String>(
				cachedPapers.size());
		for (ParsedPaper cachedPaper : cachedPapers) {
			cachedPaperFileNames.add(cachedPaper.getPaperFileName());
		}

		ExecutorService threadPool = Executors
				.newFixedThreadPool(DOCUMENT_PARSING_THREAD_POOL_SIZE);

		List<Callable<Object>> todo = new ArrayList<Callable<Object>>(
				papers.size());
		for (Paper paper : papers) {
			if (cachedPaperFileNames.contains(paper.getMetadata()
					.getPaperFileName())) {
				LOGGER.warn("Paper [" + paper.getMetadata().getPaperFileName()
						+ "] has been cached, no need to parse it again. ");
				continue;
			}

			todo.add(Executors.callable(new ParseTreeTask(paper, DB_BY_YEAR
					.get(year))));
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

		new SentenceAnalyzer().parseAndStoreAllPapers(2011);

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
