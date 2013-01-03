package edu.uic.cs.automatic_reviewer.feature.ranking;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

public class AuthorRanking extends AbstractWordOperations implements
		Constants.Ranking, Feature {

	private static final Logger LOGGER = LogHelper
			.getLogger(AuthorRanking.class);

	private static final String FIELD_NAME__RANK = "RANK";
	private static final String FIELD_NAME__NAME = "NAME";
	private static final String FIELD_NAME__ORGANIZATION = "ORGANIZATION";

	private static final File INDEX_FOLDER = new File(
			"data/index/author_ranking");

	private static final AuthorRanking INSTANCE = new AuthorRanking();

	private IndexSearcher indexSearcher;

	// singleton
	private AuthorRanking() {
		try {
			IndexReader indexReader = IndexReader.open(FSDirectory
					.open(INDEX_FOLDER));
			indexSearcher = new IndexSearcher(indexReader);
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

	}

	public static AuthorRanking getInstance() {
		return INSTANCE;
	}

	public static void main(String[] args) throws Exception {

		Assert.isTrue(false,
				"Comment this line if you want to create a new index");

		AuthorRankingCrawler crawler = new AuthorRankingCrawler();
		List<Author> authors = crawler
				.crawlAuthors(NUMBER_OF_AUTHORS_TO_RETRIEVE);

		wirteIndex(authors);
	}

	private static void wirteIndex(List<Author> authors) throws Exception {

		Directory directory = FSDirectory.open(INDEX_FOLDER);

		Map<String, Analyzer> fieldAnalyzers = new HashMap<String, Analyzer>();
		fieldAnalyzers.put(FIELD_NAME__NAME, new StandardAnalyzer(
				Version.LUCENE_36, Collections.emptySet())); // no stop-words
		fieldAnalyzers.put(FIELD_NAME__ORGANIZATION, new StandardAnalyzer(
				Version.LUCENE_36)); // default stop-words

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
				Index.ANALYZED_NO_NORMS, TermVector.NO));
		document.add(new Field(FIELD_NAME__ORGANIZATION, author
				.getOrganization(), Store.YES, Index.ANALYZED_NO_NORMS,
				TermVector.NO));

		indexWriter.addDocument(document);

		if (LOGGER.isDebugEnabled()) {
			LOGGER.info("Indexing " + document);
		}
	}

	public int getRank(Author author) {

		BooleanQuery query = new BooleanQuery();

		List<String> nameParts = standardAnalyzeWithoutRemovingStopWords(
				author.getName(), true);
		BooleanQuery nameQuery = new BooleanQuery();
		for (String name : nameParts) {
			nameQuery.add(new TermQuery(new Term(FIELD_NAME__NAME, name)),
					Occur.SHOULD);
		}

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
				return NO_RANK_VALUE;
			}

			LOGGER.debug("Score for " + author + " is ["
					+ topDocs.scoreDocs[0].score + "]");
			if (topDocs.scoreDocs[0].score < SCORE_THRESHOLD) {
				return NO_RANK_VALUE;
			}

			document = indexSearcher.doc(topDocs.scoreDocs[0].doc);

		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

		String rankString = document.get(FIELD_NAME__RANK);
		return Integer.parseInt(rankString);
	}

	@Override
	public double[] getInstanceValues(Paper paper) {

		List<Author> authors = paper.getAuthors();
		if (authors == null || authors.isEmpty()) {
			return new double[] { NO_RANK_VALUE };
		}

		int highestRank = NO_RANK_VALUE;
		for (Author author : authors) {
			int rank = getRank(author);

			// the smaller the better
			highestRank = Math.min(highestRank, rank);
		}
		return new double[] { highestRank };
	}

	@Override
	public String getName() {
		return "MAX_RANK";
	}

	@Override
	public int getNumberOfSubFeatures() {
		return 1;
	}

}
