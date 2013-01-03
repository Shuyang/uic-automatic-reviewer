package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cc.mallet.topics.ParallelTopicModel;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicPredictor;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;
import libsvm.svm_node;

public class TopicFeature {
	private static final String MODEL_CACHE_FILE = "data/model/LDAModel_2012.cache";
	private static final String FEATURE_CACHE_FILE = "data/model/LDAFeatures_2012.cache";


	TopicPredictor predictor;
	ParallelTopicModel model;
	HashMap<String, ArrayList<Double> > paperFeatureCache;




	public void learnAndSaveModel(List<Paper> papers){
		TopicPredictor predictor = new TopicPredictor();
		ParallelTopicModel model = predictor.trainTopicModel(papers, Constants.Topic.NUMBER_OF_TOPICS);
		SerializationHelper.serialize(model, MODEL_CACHE_FILE);


		paperFeatureCache = new HashMap<String, ArrayList<Double> > ();
		for(Paper p:papers){
			double[] result = predictor.predictPaper(p,model);

			ArrayList<Double> feature_i = new ArrayList<Double>();
			// use paper name is better than title, since title may be missing
			paperFeatureCache.put(p.getMetadata().getPaperFileName()/*getTitle()*/,feature_i);
			for (int j = 0; j < Constants.Topic.NUMBER_OF_TOPICS ; ++j) {
				feature_i.add(result[j]);
			}
		}
		SerializationHelper.serialize(paperFeatureCache, FEATURE_CACHE_FILE);
	}


	@SuppressWarnings("unchecked")
	public void readModelFromCache(){
		model = (ParallelTopicModel)SerializationHelper.deserialize(MODEL_CACHE_FILE);
		paperFeatureCache = (HashMap<String, ArrayList<Double>>) SerializationHelper.deserialize(FEATURE_CACHE_FILE);
		predictor = new TopicPredictor();
	}

	public ArrayList<ArrayList<svm_node>> extractTopicFeature(List<Paper> papers, int offset){

		int n = papers.size();
		int m = Constants.Topic.NUMBER_OF_TOPICS;

		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();


		for (int i = 0; i < n; ++i) {
			System.out.println(i + "/" + n);
			Paper p = papers.get(i);


			if(paperFeatureCache.containsKey(p.getMetadata().getPaperFileName()/*getTitle()*/)){
				 ArrayList<Double> paperFeature =  paperFeatureCache.get(p.getMetadata().getPaperFileName()/*getTitle()*/);
				 ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
					features.add(feature_i);
					for (int j = 0; j < m ; ++j) {
						svm_node node = new svm_node();
						node.index = j + offset;
						node.value = paperFeature.get(j);
						feature_i.add(node);
					}
			}
			else{
				System.err.println("Paper no found in the LDA feature cache");
				double[] result = predictor.predictPaper(p, model);

				ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
				features.add(feature_i);
				for (int j = 0; j < m ; ++j) {
					svm_node node = new svm_node();
					node.index = j + offset;
					node.value = result[j];
					feature_i.add(node);
				}
			}
		}
		return features;		
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		TopicFeature tf = new TopicFeature();
		List<Paper> papers = PaperCache.getInstance().getPapers(2012);
		tf.learnAndSaveModel(papers);

	}

}
