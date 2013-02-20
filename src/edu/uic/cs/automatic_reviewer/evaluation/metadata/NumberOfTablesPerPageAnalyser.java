package edu.uic.cs.automatic_reviewer.evaluation.metadata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfTablesPerPage;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class NumberOfTablesPerPageAnalyser implements Constants.Evaluation {

	private NumberOfTablesPerPage numberOfTablesPerPage = new NumberOfTablesPerPage();

	public static void main(String[] args) {
		new NumberOfTablesPerPageAnalyser().run(2011);
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

		List<Double> tableNumPerPage = getNumOfTablePerPage(positivePapers);
		System.out.println(tableNumPerPage);
		tableNumPerPage = getNumOfTablePerPage(negativePapers);
		System.out.println(tableNumPerPage);
	}

	protected List<Double> getNumOfTablePerPage(List<Paper> positivePapers) {
		List<Double> tableNumPerPage = new ArrayList<Double>();
		for (Paper paper : positivePapers) {
			double numTable = paper.getMetadata().getNumOfTables();
			double tablePerPage = numTable / paper.getNumOfPages();
			tableNumPerPage.add(tablePerPage);
		}

		Collections.sort(tableNumPerPage);
		return tableNumPerPage;
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
