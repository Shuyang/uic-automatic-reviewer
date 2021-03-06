package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.feature.term.FashionTechniques;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class FashionTechniquesFeature {
	FashionTechniques fashionTechniques;
	HashMap<String, Integer> termIndex;

	public FashionTechniquesFeature() {
		fashionTechniques = new FashionTechniques();
		termIndex = new HashMap<String, Integer>();
		int i = 0;
		for (String s : fashionTechniques.getAllFashionTechniques().keySet()) {
			termIndex.put(s, i++);
		}

	}

	public int getFeatureSize() {
		return termIndex.size();
	}

	public ArrayList<ArrayList<svm_node>> extractFancyTermsFeature(
			List<Paper> papers, int offset) {
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();

		FashionTechniques fashionTechniques = new FashionTechniques();
		for (Paper paper : papers) {
			Map<String, Boolean> techniquesMentionedInPaper = fashionTechniques
					.techniquesMentionedInPaper(paper);

			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);

			for (Entry<String, Boolean> entry : techniquesMentionedInPaper
					.entrySet()) {
				svm_node node = new svm_node();
				node.index = termIndex.get(entry.getKey()) + offset;
				if (entry.getValue()) {
					node.value = 1;
				} else {
					node.value = 0;
				}
				feature_i.add(node);
			}
		}
		return features;

	}

}
