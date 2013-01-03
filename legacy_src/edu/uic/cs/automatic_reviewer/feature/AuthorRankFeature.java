package edu.uic.cs.automatic_reviewer.feature;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import libsvm.svm_node;
import edu.uic.cs.automatic_reviewer.feature.ranking.AuthorRanking;
import edu.uic.cs.automatic_reviewer.feature.term.FashionTechniques;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;

public class AuthorRankFeature {
	FashionTechniques fashionTechniques;
	HashMap<String, Integer> termIndex;

	public AuthorRankFeature() {
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

	public static ArrayList<ArrayList<svm_node>> extractFirstAuthorRank(
			List<Paper> papers, int offset) {
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();

		AuthorRanking ranking = AuthorRanking.getInstance();
		for (Paper paper : papers) {
			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);

			svm_node node = new svm_node();
			if (paper.getAuthors().size() > 0) {
				Author author = paper.getAuthors().get(0);
				node.index = offset;
				node.value = ranking.getRank(author);
				System.out.println(" " + node.value);
			} else {
				node.index = offset;
				node.value = 0;

			}

			feature_i.add(node);
		}
		return features;

	}

	public static ArrayList<ArrayList<svm_node>> extractAuthorMaxRank(
			List<Paper> papers, int offset) {
		ArrayList<ArrayList<svm_node>> features = new ArrayList<ArrayList<svm_node>>();

		AuthorRanking ranking = AuthorRanking.getInstance();
		for (Paper paper : papers) {
			ArrayList<svm_node> feature_i = new ArrayList<svm_node>();
			features.add(feature_i);

			int maxRank = AuthorRanking.NO_RANK_VALUE;
			for (Author author : paper.getAuthors()) {
				maxRank = Math.min(maxRank, ranking.getRank(author));
			}
			svm_node node = new svm_node();
			node.index = offset;
			node.value = maxRank;
			System.out.println(" " + node.value);

			feature_i.add(node);
		}
		return features;

	}

}
