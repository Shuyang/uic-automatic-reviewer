package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.List;
import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class FeatureExtractor {

	public static enum FeatureType {
		NumOfFiguresByPage, NumOfTablesByPage, NumOfFormulasByPage, TFIDF;
	}

	private int feautreSize(FeatureType type) {
		switch (type) {
		case NumOfFiguresByPage:
		case NumOfTablesByPage:
		case NumOfFormulasByPage:
			return Constants.Feature.MAX_NUMBER_OF_PAGES_PER_PAPER;
		case TFIDF:
			return numOfTerms;
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
		default:
			return null;
		}
	}

	int numOfTerms;
	TFIDFFeature tfidfFeature;

	public svm_node[][] generateFeatureVectors(List<Paper> papers,
			List<FeatureType> types) {
		int n = papers.size();
		svm_node x[][] = new svm_node[n][];

		if (types.contains(FeatureType.TFIDF)) {
			tfidfFeature = new TFIDFFeature();
			tfidfFeature.extractIDF(papers);
			numOfTerms = tfidfFeature.getNumOfTerms();
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

		for (int i = 0; i < n; ++i) {
			ArrayList<svm_node> featureArray_i = featureArrays.get(i);
			x[i] = new svm_node[featureArray_i.size()];
			for (int j = 0; j < x[i].length; ++j) {
				x[i][j] = featureArray_i.get(j);
			}
		}

		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < x[i].length; ++j) {
				System.out.print("(" + x[i][j].index + "," + x[i][j].value
						+ ") ");
			}
			System.out.println();
		}

		return x;
	}

	public static void main(String[] args) {

	}

}
