package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import cc.mallet.topics.ParallelTopicModel;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.term.FashionTechniques;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicPredictor;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;
import libsvm.svm_node;

public class FashionTechniquesFeature {
	FashionTechniques fashionTechniques;
	HashMap<String, Integer> termIndex;
	
	
	
	public FashionTechniquesFeature() {
		fashionTechniques = new FashionTechniques();
		termIndex = new HashMap<String, Integer>();
		int i = 0;
		for(String s:fashionTechniques.getAllFashionTechniques().keySet()){
			termIndex.put(s, i++);
		}
		
	}


	public int getFeatureSize(){
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

			for (Entry<String,Boolean> entry: techniquesMentionedInPaper.entrySet()) {
				if (entry.getValue()) {
					svm_node node = new svm_node();
					node.index = termIndex.get(entry.getKey()) + offset;
					node.value = 1;
					feature_i.add(node);
				}
			}
		}
		return features;

	}

}
