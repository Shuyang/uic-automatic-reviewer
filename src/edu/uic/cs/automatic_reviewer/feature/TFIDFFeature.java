package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class TFIDFFeature extends AbstractWordOperations {
	private HashMap<Integer, Double> id_idf;
	private HashMap<String, Integer> term_id;

	public int getNumOfTerms() {
		if (term_id != null)
			return term_id.size();
		else
			return -1;
	}

	public void extractIDF(List<Paper> papers) {
		HashMap<String, Double> df = new HashMap<String, Double>();
		for (Paper p : papers) {

			if(p.getAbstract()!= null){
				List<String> termList = porterStemmingAnalyzeUsingDefaultStopWords(p
						.getAbstract());
				HashSet<String> termSet = new HashSet<String>();
				termSet.addAll(termList);
				for (String t : termSet) {
					// System.out.println(t);
					Double term_df = df.get(t);
					if (term_df == null) {
						df.put(t, 1.0);
					} else {
						df.put(t, term_df + 1);
					}
				}
				System.out.println(p.getTitle());
			}else{
				System.out.println("no abstract" +p.getTitle());
				System.out.println(p.getMetadata());
			}
		}
		int docNum = papers.size();
		int currentID = 0;

		id_idf = new HashMap<Integer, Double>();
		term_id = new HashMap<String, Integer>();
		for (Map.Entry<String, Double> entry : df.entrySet()) {
			String term = entry.getKey();
			double term_df = entry.getValue();
			if (term_df >= Constants.Feature.MIN_FREQUENCY_OF_TERMS) {
				term_id.put(term, currentID);
				id_idf.put(currentID, Math.log(docNum / term_df));
				currentID++;
			}
		}

		// for(Map.Entry<String, Integer> e:term_id.entrySet()){
		// System.out.println(e.getKey() + " "+ e.getValue() + " " +
		// id_idf.get(e.getValue()));
		// }
	}

	public ArrayList<ArrayList<svm_node>> tfidfForAllTerms(List<Paper> papers,
			int offset) {
		int n = papers.size();
		int m = id_idf.size();
		System.out.println(n + " " + m);
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();

		for (int i = 0; i < n; ++i) {
			Paper p = papers.get(i);
			

			HashMap<Integer, Double> term_TFIDFMap = new HashMap<Integer, Double>();
			
			if(p.getAbstract()!= null){
				List<String> termList = porterStemmingAnalyzeUsingDefaultStopWords(p
						.getAbstract());
				for (String t : termList) {
					// System.out.println(t);
					Integer id = term_id.get(t);
					if (id != null) {
						Double value = term_TFIDFMap.get(id);
						if (value == null)
							term_TFIDFMap.put(id, id_idf.get(id));
						else
							term_TFIDFMap.put(id, value + id_idf.get(id));
					}
				}
			}

			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);

			for (Map.Entry<Integer, Double> entry : term_TFIDFMap.entrySet()) {
				svm_node node = new svm_node();
				node.index = entry.getKey() + offset;
				node.value = entry.getValue();
				feature_i.add(node);
			}
		}

		return features;

	}
}
