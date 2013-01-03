package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.List;
import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.input.Paper;



public class FeatureExtractor  {

	public static enum FeatureType {
		NumOfFiguresByPage, NumOfTablesByPage, NumOfFormulasByPage, TFIDF, LDATopic, FashionTerms,
		FirstAuthorRank, AuthorMaxRank, ComplexityBining ,ComplexityDirectly, ComlexityNormalized;
	}

	private int feautreSize(FeatureType type) {
		switch (type) {
		case NumOfFiguresByPage:
		case NumOfTablesByPage:
		case NumOfFormulasByPage:
			return Constants.Feature.MAX_NUMBER_OF_PAGES_PER_PAPER;
		case TFIDF:
			return numOfTFIDFTerms;
		case LDATopic:
			return Constants.Topic.NUMBER_OF_TOPICS;
		case FashionTerms:
			return numOfFashionTerms;
		case FirstAuthorRank:
		case AuthorMaxRank:
			return 1;
		case ComplexityBining:
			return SentenseComplexityFeature.BiningFeatureSize;
		case ComplexityDirectly:
			return SentenseComplexityFeature.MaxComplexity;
		case ComlexityNormalized:
			return SentenseComplexityFeature.MaxComplexity;
		default:
			return 0;
		}
	}

	private ArrayList<ArrayList<svm_node>> featureComponent(List<Paper> papers,
			FeatureType type, int offset) {
		switch (type) {
		case NumOfFiguresByPage:
		case NumOfTablesByPage:
		case NumOfFormulasByPage:
			return MetaDataFeatures.extractFeatureFromMetaDataMap(papers, type,
					offset);
		case TFIDF:
			return tfidfFeature.tfidfForAllTerms(papers, offset);
		case LDATopic:
			return topicFeature.extractTopicFeature(papers, offset);
		case FashionTerms:
			return fashionTechniquesFeature.extractFancyTermsFeature(papers, offset);
		case FirstAuthorRank:
			return AuthorRankFeature.extractFirstAuthorRank(papers, offset);
		case AuthorMaxRank:
			return AuthorRankFeature.extractAuthorMaxRank(papers, offset);
		case ComplexityBining:
			return SentenseComplexityFeature.extractBiningComplexityFeature(papers, offset);
		case ComplexityDirectly:
			return SentenseComplexityFeature.extractDirectComplexityFeature(papers, offset);
		case ComlexityNormalized:
			return SentenseComplexityFeature.extractNormalizedComplexityFeature(papers, offset);
		default:
			return null;
		}
	}

	int numOfTFIDFTerms;
	int numOfFashionTerms;
	TFIDFFeature tfidfFeature;
	TopicFeature topicFeature;
	FashionTechniquesFeature fashionTechniquesFeature;
	
	private int numOfFeautres;
	

	public int getNumOfFeautres() {
		return numOfFeautres;
	}

	
	
	
	
	public svm_node[][] generateFeatureVectors(List<Paper> papers,
			List<FeatureType> types) {
		int n = papers.size();
		svm_node x[][] = new svm_node[n][];

		if (types.contains(FeatureType.TFIDF)) {
			tfidfFeature = new TFIDFFeature();
			tfidfFeature.extractIDF(papers);
			numOfTFIDFTerms = tfidfFeature.getNumOfTerms();
		}
		
		if(types.contains(FeatureType.LDATopic)){
			topicFeature = new TopicFeature();
			topicFeature.readModelFromCache();	
		}

		if (types.contains(FeatureType.FashionTerms)) {
			fashionTechniquesFeature = new FashionTechniquesFeature();
			numOfFashionTerms = fashionTechniquesFeature.getFeatureSize();
		}
		
		ArrayList<ArrayList<svm_node>> featureArrays = new ArrayList<ArrayList<svm_node>>();
		for (int i = 0; i < n; ++i) {
			featureArrays.add(new ArrayList<svm_node>());
		}
		int offset = 1;
		for (FeatureType type : types) {
			ArrayList<ArrayList<svm_node>> fc = featureComponent(papers, type,
					offset);
			for (int i = 0; i < n; ++i) {
				featureArrays.get(i).addAll(fc.get(i));
			}
			offset += feautreSize(type);
		}
		
		numOfFeautres = offset - 1;

		for (int i = 0; i < n; ++i) {
			ArrayList<svm_node> featureArray_i = featureArrays.get(i);
			x[i] = new svm_node[featureArray_i.size()];
			for (int j = 0; j < x[i].length; ++j) {
				x[i][j] = featureArray_i.get(j);
			}
		}
		
		scaleFeatures(x);

		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < x[i].length; ++j) {
				System.out.print("(" + x[i][j].index + "," + x[i][j].value
						+ ") ");
			}
			System.out.println();
		}
		
		

		return x;
	}

	static final double SCALED_MAX = 1;
	static final double SCALED_MIN = -1;
	
	public void scaleFeatures( svm_node[][] x){
		double featureMax[] = new double[numOfFeautres];
		double featureMin[] = new double[numOfFeautres];
		for(int j = 0; j < numOfFeautres; ++j){
			featureMax[j] = -Double.MAX_VALUE;
			featureMin[j] = Double.MAX_VALUE;
		}
		
		

		
		
		for(int i = 0 ; i < x.length; ++i){
			for(int j = 0; j < numOfFeautres; ++j){
				featureMax[j] = Math.max(x[i][j].value,featureMax[j]);
				featureMin[j] = Math.min(x[i][j].value,featureMin[j]);
			}
		}
		
		for(int i = 0 ; i < x.length; ++i){
			for(int j = 0; j < numOfFeautres; ++j){
				if(featureMax[j] - featureMin[j] != 0)
					x[i][j].value = (x[i][j].value - featureMin[j])*(SCALED_MAX - SCALED_MIN)/(featureMax[j]- featureMin[j]) + SCALED_MIN;
			}
		}
		
		
	}
	

}
