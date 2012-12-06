package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import cc.mallet.topics.ParallelTopicModel;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicPredictor;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import libsvm.svm_node;

public class TopicFeature {
	
	
	TopicPredictor predictor;
	ParallelTopicModel model;
	
	private void learnAndSaveModel(List<Paper> papers){
		TopicPredictor predictor = new TopicPredictor();
		ParallelTopicModel model = predictor.trainTopicModel(papers, Constants.Topic.NUMBER_OF_TOPICS);
		
		
	}
	
	
	

	public static ArrayList<ArrayList<svm_node>> extractTopicFeature(List<Paper> papers, int offset){
		
		int n = papers.size();
		int m = Constants.Topic.NUMBER_OF_TOPICS;
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();
		
		TopicPredictor predictor = new TopicPredictor();
		ParallelTopicModel model = predictor.trainTopicModel(papers,m);
		
		
		for (int i = 0; i < n; ++i) {
			Paper p = papers.get(i);
			
			double[] result = predictor.predictPaper(PaperCache.getInstance()
					.getPapers(2007).get(2), model);
			
			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);
			for (int j = 0; j < m ; ++i) {
				svm_node node = new svm_node();
				node.index = j + offset;
				node.value = result[j];
				feature_i.add(node);
			}
		}
		return features;		
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
