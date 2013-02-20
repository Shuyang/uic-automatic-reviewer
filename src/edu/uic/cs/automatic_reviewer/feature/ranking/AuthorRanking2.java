package edu.uic.cs.automatic_reviewer.feature.ranking;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.KeywordAnalyzer;
import org.apache.lucene.analysis.PerFieldAnalyzerWrapper;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.Field.Index;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.Field.TermVector;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;

import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

public class AuthorRanking2 extends AbstractWordOperations implements
		Constants.Ranking, Feature {

	private static final Logger LOGGER = LogHelper
			.getLogger(AuthorRanking2.class);

	private static final String FIELD_NAME__RANK = "RANK"; // store only
	private static final String FIELD_NAME__NAME = "NAME"; // store only

	private static final String FIELD_NAME__NAME_FIRST = "NAME_FIRST";
	private static final String FIELD_NAME__NAME_LAST = "NAME_LAST";
	private static final String FIELD_NAME__NAME_MIDDLE = "NAME_MIDDLE";
	private static final String FIELD_NAME__ORGANIZATION = "ORGANIZATION";

	private static final Map<String, Float> WEIGHTS_BY_FILED = new HashMap<String, Float>();
	static {
		WEIGHTS_BY_FILED.put(FIELD_NAME__NAME_LAST, 1f);
		WEIGHTS_BY_FILED.put(FIELD_NAME__NAME_FIRST, 0.75f);
		WEIGHTS_BY_FILED.put(FIELD_NAME__NAME_MIDDLE, 0.5f);
		WEIGHTS_BY_FILED.put(FIELD_NAME__ORGANIZATION, 1f);
	}

	private static final File INDEX_FOLDER = new File(
			"data/index/author_ranking_2");

	private static volatile AuthorRanking2 INSTANCE = null;

	private IndexSearcher indexSearcher;

	// singleton make sure only one IndexReader created
	private AuthorRanking2() {
		try {
			IndexReader indexReader = IndexReader.open(FSDirectory
					.open(INDEX_FOLDER));
			indexSearcher = new IndexSearcher(indexReader);
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

	}

	// private static float THRESHOLD = 0;

	public static synchronized AuthorRanking2 getInstance(/* float threshold */) {
		// THRESHOLD = threshold;
		if (INSTANCE == null) {
			INSTANCE = new AuthorRanking2();
		}
		return INSTANCE;
	}

	public static void createIndex() {
		Assert.isTrue(false,
				"Comment this line if you want to create a new index");

		AuthorRankingCrawler crawler = new AuthorRankingCrawler();
		List<Author> authors = crawler
				.crawlAuthors(NUMBER_OF_AUTHORS_TO_RETRIEVE);

		try {
			wirteIndex(authors);
		} catch (Exception e) {
			throw new AutomaticReviewerException(e);
		}
	}

	private static void wirteIndex(List<Author> authors) throws Exception {

		Directory directory = FSDirectory.open(INDEX_FOLDER);

		Map<String, Analyzer> fieldAnalyzers = new HashMap<String, Analyzer>();

		fieldAnalyzers.put(FIELD_NAME__NAME_MIDDLE, new StandardAnalyzer(
				Version.LUCENE_36, Collections.emptySet())); // no stop-words
		fieldAnalyzers.put(FIELD_NAME__ORGANIZATION, new StandardAnalyzer(
				Version.LUCENE_36)); // default stop-words

		// other fields are all using KeywordAnalyzer
		Analyzer analyzer = new PerFieldAnalyzerWrapper(new KeywordAnalyzer(),
				fieldAnalyzers);

		IndexWriterConfig config = new IndexWriterConfig(Version.LUCENE_36,
				analyzer);
		config.setOpenMode(OpenMode.CREATE);

		IndexWriter indexWriter = new IndexWriter(directory, config);

		int rank = 0;
		for (Author author : authors) {
			indexAuthor(indexWriter, author, ++rank);
		}

		indexWriter.close();
	}

	private static void indexAuthor(IndexWriter indexWriter, Author author,
			int rank) throws CorruptIndexException, IOException {

		Document document = new Document();

		document.add(new Field(FIELD_NAME__RANK, rank + "", Store.YES,
				Index.NOT_ANALYZED_NO_NORMS, TermVector.NO));
		document.add(new Field(FIELD_NAME__NAME, author.getName(), Store.YES,
				Index.NOT_ANALYZED_NO_NORMS, TermVector.NO));

		// /////////////////////////////////////////////////////////////////////
		String[] first_middle_last = parseName(author.getName());
		String firstName = first_middle_last[0];
		String middleName = first_middle_last[1];
		String lastName = first_middle_last[2];

		if (firstName != null && !firstName.trim().isEmpty()) {
			Field firstNameField = new Field(FIELD_NAME__NAME_FIRST, firstName,
					Store.NO, Index.ANALYZED, TermVector.NO);
			firstNameField.setBoost(WEIGHTS_BY_FILED
					.get(FIELD_NAME__NAME_FIRST));
			document.add(firstNameField);
		}

		if (middleName != null && !middleName.trim().isEmpty()) {
			Field middleNameField = new Field(FIELD_NAME__NAME_MIDDLE,
					middleName, Store.NO, Index.ANALYZED, TermVector.NO);
			middleNameField.setBoost(WEIGHTS_BY_FILED
					.get(FIELD_NAME__NAME_MIDDLE));
			document.add(middleNameField);
		}

		if (lastName != null && !lastName.trim().isEmpty()) {
			Field lastNameField = new Field(FIELD_NAME__NAME_LAST, lastName,
					Store.NO, Index.ANALYZED, TermVector.NO);
			lastNameField.setBoost(WEIGHTS_BY_FILED.get(FIELD_NAME__NAME_LAST));
			document.add(lastNameField);
		}

		// /////////////////////////////////////////////////////////////////////
		Field organizationField = new Field(FIELD_NAME__ORGANIZATION,
				author.getOrganization(), Store.YES, Index.ANALYZED,
				TermVector.NO);
		organizationField.setBoost(WEIGHTS_BY_FILED
				.get(FIELD_NAME__ORGANIZATION));
		document.add(organizationField);

		indexWriter.addDocument(document);

		if (LOGGER.isDebugEnabled()) {
			LOGGER.info("Indexing " + document);
		}
	}

	private static String[] parseName(String name) {
		// in lower-case
		String[] nameParts = StringUtils.split(name.toLowerCase());
		String firstName = nameParts[0];
		String lastName = nameParts[nameParts.length - 1];

		StringBuilder middleNameBuilder = new StringBuilder();
		for (int index = 1; index < nameParts.length - 1; index++) {
			middleNameBuilder.append(nameParts[index]).append(" ");
		}
		String middleName = middleNameBuilder.toString().trim();

		return new String[] { firstName, middleName, lastName };
	}

	public void printAllAuthors() throws Exception {
		for (int i = 0; i < indexSearcher.maxDoc(); i++) {
			Document document = indexSearcher.doc(i);
			String rankString = document.get(FIELD_NAME__RANK);
			String name = document.get(FIELD_NAME__NAME);
			String organization = document.get(FIELD_NAME__ORGANIZATION);
			System.out.println(rankString + "\t" + name + "\t" + organization);
		}
	}

	private Document retrieveRecord(Author author) {
		BooleanQuery query = new BooleanQuery();

		String[] nameParts = parseName(author.getName());
		BooleanQuery nameQuery = new BooleanQuery();
		nameQuery.add(new TermQuery(new Term(FIELD_NAME__NAME_FIRST,
				nameParts[0])), Occur.SHOULD);

		List<String> middleNameParts = standardAnalyzeWithoutRemovingStopWords(
				nameParts[1], true);
		for (String middleNamePart : middleNameParts) {
			nameQuery.add(new TermQuery(new Term(FIELD_NAME__NAME_MIDDLE,
					middleNamePart)), Occur.SHOULD);
		}

		nameQuery.add(new TermQuery(new Term(FIELD_NAME__NAME_LAST,
				nameParts[2])), Occur.SHOULD);

		List<String> organizationParts = standardAnalyzeUsingDefaultStopWords(author
				.getOrganization());
		BooleanQuery organizationQuery = new BooleanQuery();
		for (String org : organizationParts) {
			organizationQuery.add(new TermQuery(new Term(
					FIELD_NAME__ORGANIZATION, org)), Occur.SHOULD);
		}

		query.add(nameQuery, Occur.MUST);
		query.add(organizationQuery, Occur.MUST);

		Document document = null;
		try {
			TopDocs topDocs = indexSearcher.search(query, 1);

			if (topDocs.totalHits == 0) {
				LOGGER.info("No record for " + author);
				return null;
			}

			LOGGER.debug("Score for " + author + " is ["
					+ topDocs.scoreDocs[0].score + "]");
			if (topDocs.scoreDocs[0].score < /* THRESHOLD */SCORE_THRESHOLD2) {
				return null;
			}

			document = indexSearcher.doc(topDocs.scoreDocs[0].doc);

		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

		return document;
	}

	public int getRank(Author author) {
		Document document = retrieveRecord(author);
		if (document == null) {
			return NO_RANK_VALUE;
		}

		String rankString = document.get(FIELD_NAME__RANK);
		return Integer.parseInt(rankString);
	}

	/**
	 * For test
	 */
	String getName(Author author) {
		Document document = retrieveRecord(author);
		if (document == null) {
			return null;
		}

		return document.get(FIELD_NAME__NAME);
	}

	/**
	 * For test
	 */
	String getOrganization(Author author) {
		Document document = retrieveRecord(author);
		if (document == null) {
			return null;
		}

		return document.get(FIELD_NAME__ORGANIZATION);
	}

	@Override
	public double[] getInstanceValues(Paper paper) {

		List<Author> authors = paper.getAuthors();
		if (authors == null || authors.isEmpty()) {
			return new double[] { Double.NaN };
		}

		int highestRank = NO_RANK_VALUE;
		for (Author author : authors) {
			int rank = getRank(author);

			// the smaller the better
			highestRank = Math.min(highestRank, rank);
		}

		if (highestRank == NO_RANK_VALUE) {
			return new double[] { Double.NaN };
		}
		return new double[] { highestRank };
	}

	@Override
	public int getNumberOfSubFeatures() {
		return 1;
	}

	@Override
	public String[] getSubFeatureNames() {
		return new String[] { getName() };
	}

	@Override
	public String getName() {
		return "MAX_RANK";
	}

}