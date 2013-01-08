package edu.uic.cs.automatic_reviewer.evaluation.metadata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfTablesPerPage;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class NumberOfTablesPerPageAnalyser {

	private int MIN_ACCEPTED_PAPER_PAGE_NUMBER = 8;
	private NumberOfTablesPerPage numberOfTablesPerPage = new NumberOfTablesPerPage();

	public static void main(String[] args) {
		new NumberOfTablesPerPageAnalyser().run(2010);
	}

	public void run(int year) {
		List<Paper> positivePapers = PaperCache.getInstance().getPapers(year,
				PaperPublishType.LongPaper);
		List<Paper> negativePapersOriginal = PaperCache.getInstance()
				.getPapers(year, PaperPublishType.WorkshopPaper,
						PaperPublishType.StudentWorkshopPaper);

		List<Paper> negativePapers = new ArrayList<Paper>();
		for (Paper paper : negativePapersOriginal) {
			if (paper.getNumOfPages() < MIN_ACCEPTED_PAPER_PAGE_NUMBER) {
				continue;
			}

			negativePapers.add(paper);
		}

		double[] positiveResult = countAverageTableFrequencies(positivePapers);
		double[] negativeResult = countAverageTableFrequencies(negativePapers);

		System.out.println(Arrays.toString(positiveResult));
		System.out.println(Arrays.toString(negativeResult));
	}

	private double[] countAverageTableFrequencies(List<Paper> papers) {

		double[] result = new double[numberOfTablesPerPage
				.getNumberOfSubFeatures()];
		for (Paper paper : papers) {
			double[] counts = numberOfTablesPerPage.getInstanceValues(paper);
			Assert.isTrue(counts.length == result.length);

			for (int index = 0; index < counts.length; index++) {
				result[index] += counts[index];
			}
		}

		for (int index = 0; index < result.length; index++) {
			result[index] /= papers.size();
		}
		return result;
	}
}
