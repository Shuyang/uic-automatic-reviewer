package edu.uic.cs.automatic_reviewer.feature.term;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.log4j.Logger;

import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

abstract class TermsTFIDF extends AbstractWordOperations implements Feature {

	private static final Logger LOGGER = LogHelper.getLogger(TermsTFIDF.class);

	private Map<String, Double> idfByTerm;

	protected TermsTFIDF() {
		Map<String, Double> dfByTerm = new HashMap<String, Double>();

		List<Paper> allPapers = PaperCache.getInstance().getAllPapers();
		int docNum = 0;
		for (Paper paper : allPapers) {
			String termsString = getTermsString(paper);
			if (termsString == null || termsString.trim().isEmpty()) {
				continue;
			}

			docNum++;
			Set<String> terms = new HashSet<String>(
					porterStemmingAnalyzeUsingDefaultStopWords(termsString));

			for (String term : terms) {
				Double frequency = dfByTerm.get(term);
				dfByTerm.put(term, frequency == null ? 1 : (frequency + 1));
			}
		}

		idfByTerm = new HashMap<String, Double>();
		for (Entry<String, Double> entry : dfByTerm.entrySet()) {
			String term = entry.getKey();
			double term_df = entry.getValue();

			idfByTerm.put(term, Math.log(docNum / term_df));
		}
	}

	protected abstract String getTermsString(Paper paper);

	@Override
	public double[] getInstanceValues(Paper paper) {
		String title = getTermsString(paper);
		if (title == null || title.trim().isEmpty()) {
			// missing data
			return new double[] { Double.NaN };
		}

		List<String> terms = porterStemmingAnalyzeUsingDefaultStopWords(title);
		Map<String, Integer> tfByTerm = new HashMap<String, Integer>();
		for (String term : terms) {
			Integer frequency = tfByTerm.get(term);
			tfByTerm.put(term, frequency == null ? 1 : (frequency + 1));
		}

		double maxTFIDF = 0;
		for (Entry<String, Integer> entry : tfByTerm.entrySet()) {
			String term = entry.getKey();
			Double idf = idfByTerm.get(term);
			if (idf == null) {
				LOGGER.info("No term ["
						+ term
						+ "] exists in call training data, it must be a prediction instance. ");
				continue;
			}

			double tf = entry.getValue().doubleValue();
			double tfidf = tf * idf.doubleValue();

			maxTFIDF = Math.max(tfidf, maxTFIDF);
		}

		return new double[] { maxTFIDF };
	}

	@Override
	public int getNumberOfSubFeatures() {
		return 1;
	}

	@Override
	public String[] getSubFeatureNames() {
		return new String[] { getName() };
	}

}
