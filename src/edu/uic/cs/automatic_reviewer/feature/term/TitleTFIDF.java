package edu.uic.cs.automatic_reviewer.feature.term;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;

public class TitleTFIDF extends AbstractWordOperations implements Feature {

	private Map<String, Double> idfByTerm;

	public TitleTFIDF() {
		Map<String, Double> dfByTerm = new HashMap<String, Double>();

		List<Paper> allPapers = PaperCache.getInstance().getAllPapers();
		int docNum = 0;
		for (Paper paper : allPapers) {
			String title = paper.getTitle();
			if (title == null || title.trim().isEmpty()) {
				continue;
			}

			docNum++;
			Set<String> terms = new HashSet<String>(
					porterStemmingAnalyzeUsingDefaultStopWords(title));

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

	@Override
	public double[] getInstanceValues(Paper paper) {
		String title = paper.getTitle();
		if (title == null || title.trim().isEmpty()) {
			return new double[] { 0 };
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
			double tf = entry.getValue().doubleValue();
			double tfidf = tf * idfByTerm.get(term);

			maxTFIDF = Math.max(tfidf, maxTFIDF);
		}

		return new double[] { maxTFIDF };
	}

	@Override
	public String getName() {
		return "TITLE";
	}

	@Override
	public int getNumberOfSubFeatures() {
		return 1;
	}

}
