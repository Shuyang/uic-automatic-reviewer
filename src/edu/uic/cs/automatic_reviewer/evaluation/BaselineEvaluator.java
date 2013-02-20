package edu.uic.cs.automatic_reviewer.evaluation;

import java.util.List;
import java.util.Random;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution.Year;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;

public class BaselineEvaluator implements Constants.Evaluation {

	private static final Year YEAR = Year._All;
	private static final Random RANDOM = new Random();

	public static void main(String[] args) {
		for (int index = 0; index < 99; index++) {
			iteration();
		}
	}

	private static void iteration() {

		List<Paper> papersOfPos = null;
		List<Paper> papersOfNeg = null;

		if (YEAR != Year._All) {
			papersOfPos = PaperCache.getInstance().getPapers(YEAR.getYears(),
					PaperPublishType.LongPaper);
			papersOfNeg = PaperCache.getInstance().getPapers(YEAR.getYears(),
					PaperPublishType.WorkshopPaper,
					PaperPublishType.StudentWorkshopPaper);
		} else {
			papersOfPos = PaperCache.getInstance().getAllPapers(
					PaperPublishType.LongPaper);
			papersOfNeg = PaperCache.getInstance().getAllPapers(
					PaperPublishType.WorkshopPaper,
					PaperPublishType.StudentWorkshopPaper);
		}

		int posNum = papersOfPos.size();
		int negNum = 0;
		for (Paper negativePaper : papersOfNeg) {
			if (negativePaper.getNumOfPages() < MIN_ACCEPTED_PAPER_PAGE_NUMBER) {
				continue;
			}

			negNum++;
		}
		double ratio = ((double) posNum) / (posNum + negNum);

		// System.out.println(posNum);
		// System.out.println(negNum);
		//
		// System.out.println(ratio);

		int tp = 0;
		int fp = 0;
		int tn = 0;
		int fn = 0;
		// positive
		for (int index = 0; index < posNum; index++) {
			// System.out.println(randomValue);
			if (RANDOM.nextDouble() < ratio) {
				// assign true
				tp++;
			} else {
				fn++;
			}
		}

		// negative
		for (int index = 0; index < negNum; index++) {
			// System.out.println(randomValue);
			if (RANDOM.nextDouble() < ratio) {
				// assign true
				fp++;
			} else {
				tn++;
			}
		}

		double p = ((double) tp) / (tp + fp);
		double r = ((double) tp) / (tp + fn);
		double f = 2 * p * r / (p + r);
		System.out.println(p + "\t" + r + "\t" + f + "\t" + (tp + tn) + "\t"
				+ (fp + fn));
	}
}
