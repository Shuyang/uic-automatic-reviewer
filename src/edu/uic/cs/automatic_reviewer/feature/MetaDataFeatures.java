package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import libsvm.svm_node;

import edu.uic.cs.automatic_reviewer.input.Metadata;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class MetaDataFeatures {
	

	
	public static ArrayList<ArrayList<svm_node>> extractFeatureFromMetaDataMap(List<Paper> papers, FeatureExtractor.FeatureType type, int offset){
		int n = papers.size();
		int m = Constants.MAX_NUMBER_OF_PAGES_PER_PAPER;
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();

		for(int i = 0; i < n; ++i){
			Paper p = papers.get(i);
			Map<Integer,Integer> pageMap = MapFromMetaData(p.getMetadata(), type);
			//System.out.println(pageMap.size());
			
			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);
			
			for(int j = 0; j < m; ++j){
				Integer num = pageMap.get(j);
				if(num != null && num != 0){
					svm_node node = new svm_node();
					node.index = j + offset;
					node.value = num;
					feature_i.add(node);
				}
			}
		}
		return features;  
		
	}
	
	private static Map<Integer,Integer> MapFromMetaData (Metadata m,
			FeatureExtractor.FeatureType type){
		switch(type){
		case NumOfFiguresByPage: return m.getNumOfFiguresByPage();
		case NumOfTablesByPage: return m.getNumOfTablesByPage();
		case NumOfFormulasByPage: return m.getNumOfFormulasByPage();
		default: return null;
		}
	}
}
