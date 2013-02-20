package edu.uic.cs.automatic_reviewer.evaluation.metadata;

import java.util.ArrayList;
import java.util.List;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfFormulasPerPage;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class NumberOfFormulasPerPageAnalyser implements Constants.Evaluation {

	private NumberOfFormulasPerPage numberOfFormulasPerPage = new NumberOfFormulasPerPage();

	public static void main(String[] args) {
		new NumberOfFormulasPerPageAnalyser().run(2007);
		System.out.println();

		new NumberOfFormulasPerPageAnalyser().run(2010);
		System.out.println();

		new NumberOfFormulasPerPageAnalyser().run(2011);
		System.out.println();

		new NumberOfFormulasPerPageAnalyser().run(2012);
		System.out.println();
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

		for (double resut : positiveResult) {
			System.out.print(resut + "\t");
		}
		System.out.println();

		for (double resut : negativeResult) {
			System.out.print(resut + "\t");
		}
		System.out.println();

		// List<Double> tableNumPerPage = getNumOfTablePerPage(positivePapers);
		// System.out.println(tableNumPerPage);
		// tableNumPerPage = getNumOfTablePerPage(negativePapers);
		// System.out.println(tableNumPerPage);
	}

	// protected List<Double> getNumOfTablePerPage(List<Paper> positivePapers) {
	// List<Double> tableNumPerPage = new ArrayList<Double>();
	// for (Paper paper : positivePapers) {
	// double numTable = paper.getMetadata().getNumOfTables();
	// double tablePerPage = numTable / paper.getNumOfPages();
	// tableNumPerPage.add(tablePerPage);
	// }
	//
	// Collections.sort(tableNumPerPage);
	// return tableNumPerPage;
	// }

	private double[] countAverageTableFrequencies(List<Paper> papers) {

		double[] result = new double[numberOfFormulasPerPage
				.getNumberOfSubFeatures()];
		for (Paper paper : papers) {
			double[] counts = numberOfFormulasPerPage.getInstanceValues(paper);
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
