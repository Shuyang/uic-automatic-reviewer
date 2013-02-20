package edu.uic.cs.automatic_reviewer.feature.sentence;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;

import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceAnalyzer.ParsedPaper;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class ObjectDBHelper {

	// private static final String SOURCE_DB_NAME =
	// "object_db/paper_parse_tree_2011.part.db4o";
	private static final String _DESTINATION_DB_NAME = "object_db/paper_parse_tree_$YEAR$.db4o";

	public static void main(String[] args) throws IOException {

		// checkRedundancy(2007);
		// checkRedundancy(2010);
		// checkRedundancy(2011);
		// checkRedundancy(2012);
		printPaperNotInDB(2007);
		printPaperNotInDB(2010);
		printPaperNotInDB(2011);
		printPaperNotInDB(2012);
	}

	private static void checkRedundancy(int year) {

		String destinationDBName = _DESTINATION_DB_NAME.replace("$YEAR$",
				("" + year));
		ObjectContainer destinationDB = Db4oEmbedded
				.openFile(destinationDBName);

		ObjectSet<ParsedPaper> papersInDB = destinationDB
				.query(ParsedPaper.class);
		Set<String> paperFileNames = new HashSet<String>(papersInDB.size());
		for (ParsedPaper paperInDB : papersInDB) {
			Assert.isTrue(paperFileNames.add(paperInDB.getPaperFileName()),
					paperInDB.getPaperFileName());
		}

		destinationDB.close();

		System.out.println(year + " is OK. ");
	}

	private static void printPaperNotInDB(int year) {

		String destinationDBName = _DESTINATION_DB_NAME.replace("$YEAR$",
				("" + year));
		ObjectContainer destinationDB = Db4oEmbedded
				.openFile(destinationDBName);

		Set<String> paperFileNamesInDB = new TreeSet<String>();
		ObjectSet<ParsedPaper> papersInDB = destinationDB
				.query(ParsedPaper.class);
		for (ParsedPaper paperInDB : papersInDB) {
			paperFileNamesInDB.add(paperInDB.getPaperFileName());

			if (paperInDB.getAbstractParseTrees().isEmpty()
					&& (paperInDB.getContentSentenceTrees().isEmpty() || paperInDB
							.getContentSentenceTrees().get(0).isEmpty())) {
				System.out.println("Missing content: "
						+ paperInDB.getPaperFileName());
			}
		}

		for (Paper paper : PaperCache.getInstance().getPapers(year)) {
			if (!paperFileNamesInDB.contains(paper.getMetadata()
					.getPaperFileName())) {
				System.out.println("Missing paper: "
						+ paper.getMetadata().getPaperFileName());
			}
		}

		destinationDB.close();

		System.out.println("**************************************");
	}
	// private static void merge() {
	// ObjectContainer sourceDB = Db4oEmbedded.openFile(SOURCE_DB_NAME);
	// ObjectSet<ParsedPaper> source = sourceDB.query(ParsedPaper.class);
	//
	// merge(source, 2007);
	// merge(source, 2010);
	// merge(source, 2011);
	// merge(source, 2012);
	//
	// sourceDB.close();
	// }
	//
	// private static void merge(ObjectSet<ParsedPaper> source, int year) {
	//
	// String destinationDBName = _DESTINATION_DB_NAME.replace("$YEAR$",
	// ("" + year));
	// ObjectContainer destinationDB = Db4oEmbedded
	// .openFile(destinationDBName);
	//
	// List<Paper> papers = PaperCache.getInstance().getPapers(year);
	// Assert.notEmpty(papers);
	//
	// Set<String> paperFileNames = new HashSet<String>(papers.size());
	// for (Paper paper : papers) {
	// paperFileNames.add(paper.getMetadata().getPaperFileName());
	// }
	//
	// for (ParsedPaper paper : source) {
	// String paperFileName = paper.getPaperFileName();
	//
	// if (paperFileNames.contains(paperFileName)) {
	// destinationDB.store(paper);
	// System.out.println(paper.getPaperFileName());
	// }
	// }
	//
	// destinationDB.commit();
	// destinationDB.close();
	// System.out.println("******************************************\n");
	// }
}
