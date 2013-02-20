package edu.uic.cs.automatic_reviewer.feature.ranking;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;
import edu.uic.cs.automatic_reviewer.input.PaperCache;

class AuthorRankingThresholdTuner {

	// 2.3 is the best
	/**
	 * threshold : 2.3 <br>
	 * 440|40/2693 <br>
	 * precision: 0.9166666666666666 <br>
	 * recall: 0.16338655774229485 <br>
	 * f1: 0.2773400567286479
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		for (float threshold = 3; threshold <= 5; threshold += 0.1F) {
			System.out.println("threshold : " + threshold);
			run(threshold);
			System.out.println("===========================================");
		}

	}

	public static void main2(String[] args) {
		float threshold = 0;
		for (int index = 0; index < 10; index++) {
			threshold = index;
			System.out.println("threshold : " + threshold);
			run(threshold);

			System.out.println("===========================================");
		}

	}

	public static void run(float threshold) {
		List<Paper> papers = PaperCache.getInstance().getAllPapers();
		Collections.sort(papers, new Comparator<Paper>() {
			@Override
			public int compare(Paper o1, Paper o2) {
				return o1.getMetadata().getPaperFileName()
						.compareTo(o2.getMetadata().getPaperFileName());
			}
		});

		int totalNamesNumInPaper = 0;
		int totalCorrect = 0;
		int totalError = 0;
		for (Paper paper : papers) {

			List<Author> authors = paper.getAuthors();
			// System.out.print(paper.getMetadata().getPaperFileName() + "\t");

			List<String> namesFromPaper = new ArrayList<String>();
			for (Author author : authors) {
				namesFromPaper.add(author.getName());
			}

			List<String> namesFromIndex = new ArrayList<String>();
			for (Author author : authors) {
				String name = AuthorRanking.getInstance(/*threshold*/).getName(
						author);
				namesFromIndex.add(name);
			}

			// ////////////////////////////////////////////////////////////////

			for (int index = 0; index < namesFromPaper.size(); index++) {
				// String nameFromPaper = namesFromPaper.get(index);
				totalNamesNumInPaper++;
				// System.out.print(nameFromPaper + "\t|\t");
			}
			// System.out.println();

			// System.out.print("\t\t");
			int correctInPaper = 0;
			int errorInPaper = 0;
			for (int index = 0; index < namesFromIndex.size(); index++) {
				String nameFromIndex = namesFromIndex.get(index);
				String nameFromPaper = namesFromPaper.get(index);
				if (areSameNames(nameFromIndex, nameFromPaper)) {
					correctInPaper++;
				} else if (nameFromIndex != null) {
					errorInPaper++;
				}

				// System.out.print(((nameFromIndex == null) ? "\t"
				// : nameFromIndex) + "\t|\t");
			}

			// System.out.println(correctInPaper + "|" + errorInPaper + "/"
			// + namesFromIndex.size());
			totalCorrect += correctInPaper;
			totalError += errorInPaper;

			// System.out.println("==========================");
		}

		// System.out.println();
		System.out.println(totalCorrect + "|" + totalError + "/"
				+ totalNamesNumInPaper);

		double precision = ((double) totalCorrect)
				/ (totalCorrect + totalError);
		double recall = ((double) totalCorrect) / totalNamesNumInPaper;
		double f1 = 2.0 * precision * recall / (precision + recall);

		System.out.println("precision: " + precision);
		System.out.println("recall: " + recall);
		System.out.println("f1: " + f1);
	}

	private static boolean areSameNames(String name1, String name2) {
		if (name1 == null || name2 == null) {
			return false;
		}

		name1 = name1.toLowerCase();
		name2 = name2.toLowerCase();

		String[] name1_parts = StringUtils.split(name1);
		String[] name2_parts = StringUtils.split(name2);

		int name1_length = name1_parts.length;
		int name2_length = name2_parts.length;

		int largerLength = Math.max(name1_length, name2_length);
		int smallerLength = Math.min(name1_length, name2_length);
		String[] largerParts = (largerLength == name1_length) ? name1_parts
				: name2_parts;
		String[] smallerParts = (largerParts == name1_parts) ? name2_parts
				: name1_parts;

		Set<String> largerSet = new HashSet<String>(Arrays.asList(largerParts));
		Set<String> smallerSet = new HashSet<String>(
				Arrays.asList(smallerParts));

		if (smallerLength < 2) {
			return largerSet.containsAll(smallerSet);
		} else if (smallerLength == 2 && largerLength == 2) {
			return largerSet.containsAll(smallerSet);
		} else {
			int minSamePartCount = largerLength - 1;
			int sameCount = 0;
			for (String smallerPart : smallerSet) {
				if (largerSet.contains(smallerPart)) {
					sameCount++;
				}

				if (sameCount >= minSamePartCount) {
					return true;
				}
			}

			return false;
		}
	}
}
