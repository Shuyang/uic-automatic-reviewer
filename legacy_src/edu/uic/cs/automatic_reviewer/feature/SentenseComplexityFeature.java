package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceComplexity;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class SentenseComplexityFeature {
	final static int BiningStepLength = 5;
	final static int BiningFeatureSize = 6;

	static public ArrayList<ArrayList<svm_node>> extractBiningComplexityFeature(
			List<Paper> papers, int offset) {
		SentenceComplexity sentenceComplexity = new SentenceComplexity();

		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();
		for (Paper paper : papers) {
			TreeMap<Integer, Integer> complexity = sentenceComplexity
					.measurePaperSentenceComplexity(paper);
			int biningFeatures[] = new int[BiningFeatureSize];
			for (int i = 0; i < BiningFeatureSize; ++i) {
				biningFeatures[i] = 0;
			}
			for (Entry<Integer, Integer> entry : complexity.entrySet()) {
				if (entry.getKey() / BiningStepLength < BiningFeatureSize) {
					biningFeatures[entry.getKey() / BiningStepLength] += entry
							.getValue();
				} else {
					System.err.println("warning: complexity out of max bin");
					biningFeatures[BiningFeatureSize - 1] += 1;
				}
			}

			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);
			for (int i = 0; i < BiningFeatureSize; ++i) {
				svm_node node = new svm_node();
				node.index = i + offset;
				node.value = biningFeatures[i];
				feature_i.add(node);
			}
		}
		return features;
	}

	final static int MaxComplexity = 40;

	static public ArrayList<ArrayList<svm_node>> extractDirectComplexityFeature(
			List<Paper> papers, int offset) {
		SentenceComplexity sentenceComplexity = new SentenceComplexity();

		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();
		for (Paper paper : papers) {
			TreeMap<Integer, Integer> complexity = sentenceComplexity
					.measurePaperSentenceComplexity(paper);
			int frequencys[] = new int[MaxComplexity];

			for (Entry<Integer, Integer> entry : complexity.entrySet()) {
				if (entry.getKey() < MaxComplexity) {
					frequencys[entry.getKey()] += entry.getValue();
				} else {
					System.err.println("warning: frequency out of max bin");
					frequencys[MaxComplexity - 1] += entry.getValue();
				}
			}

			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);
			for (int i = 0; i < MaxComplexity; ++i) {
				svm_node node = new svm_node();
				node.index = i + offset;
				node.value = frequencys[i];
				feature_i.add(node);
			}
		}
		return features;
	}

	static public ArrayList<ArrayList<svm_node>> extractNormalizedComplexityFeature(
			List<Paper> papers, int offset) {
		SentenceComplexity sentenceComplexity = new SentenceComplexity();

		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();
		for (Paper paper : papers) {
			TreeMap<Integer, Integer> complexity = sentenceComplexity
					.measurePaperSentenceComplexity(paper);
			int frequencys[] = new int[MaxComplexity];

			int total = 0;
			for (Entry<Integer, Integer> entry : complexity.entrySet()) {
				if (entry.getKey() < MaxComplexity) {
					frequencys[entry.getKey()] += entry.getValue();
					total += entry.getValue();
				} else {
					System.err.println("warning: frequency out of max bin");
					frequencys[MaxComplexity - 1] += entry.getValue();
					total += entry.getValue();
				}
			}
			if (total == 0)
				total = 1;

			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);
			for (int i = 0; i < MaxComplexity; ++i) {
				svm_node node = new svm_node();
				node.index = i + offset;
				node.value = frequencys[i] / ((double) total);
				feature_i.add(node);
			}
		}
		return features;
	}

}
