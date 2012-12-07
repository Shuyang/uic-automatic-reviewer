package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class TFIDFFeature extends AbstractWordOperations {
	//private HashMap<Integer, Double> id_idf;
	private HashMap<String, Double> term_idf;

	public int getNumOfTerms() {
		if (term_idf != null)
			return term_idf.size();
		else
			return -1;
	}

	public void extractIDF(List<Paper> papers) {
		HashMap<String, Double> df = new HashMap<String, Double>();
		for (Paper p : papers) {

			if(p.getAbstract()!= null){
				List<String> termList = porterStemmingAnalyzeUsingDefaultStopWords(p
						.getTitle());
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
				System.out.println("no title" +p.getTitle());
				System.out.println(p.getMetadata());
			}
		}
		int docNum = papers.size();


		term_idf = new HashMap<String, Double>();
		for (Map.Entry<String, Double> entry : df.entrySet()) {
			String term = entry.getKey();
			double term_df = entry.getValue();
			if (term_df >= Constants.Feature.MIN_FREQUENCY_OF_TERMS) {
				term_idf.put(term, Math.log(docNum / term_df));
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
		int m = term_idf.size();
		//System.out.println(n + " " + m);
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();

		for (int i = 0; i < n; ++i) {
			Paper p = papers.get(i);


			HashMap<String,Integer> termCount = new HashMap<String,Integer>();

			if(p.getTitle()!= null){
				List<String> termList = porterStemmingAnalyzeUsingDefaultStopWords(p
						.getTitle());

				for(String s: termList){
					if(termCount.containsKey(s))
						termCount.put(s, termCount.get(s)+1);
					else
						termCount.put(s, 1);
				}
			}

			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);

			//System.out.println(termCount.size() + "##");
			int j = 0;
			for(Entry<String,Double> entry:term_idf.entrySet()){
				svm_node node = new svm_node();
				node.index = (j++) + offset;



				if(termCount.containsKey(entry.getKey())){
					node.value = entry.getValue()*termCount.get(entry.getKey());
				}
				else
					node.value = 0;
				feature_i.add(node);

			}

		}

		return features;

	}
}
