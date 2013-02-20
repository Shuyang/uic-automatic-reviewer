package edu.uic.cs.automatic_reviewer.evaluation.ranking;

import java.util.ArrayList;
import java.util.List;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.ranking.AuthorRanking;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution.Year;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;

public class AuthorRankingAnalyser implements Constants.Evaluation {

	private static final Year YEAR = Year._2012;

	public static void main(String[] args) {

		List<Paper> positivePapers = PaperCache.getInstance().getPapers(
				YEAR.getYears(), PaperPublishType.LongPaper);
		List<Paper> allNegativePapers = PaperCache.getInstance().getPapers(
				YEAR.getYears(), PaperPublishType.WorkshopPaper,
				PaperPublishType.StudentWorkshopPaper);
		List<Paper> negativePapers = new ArrayList<Paper>();
		for (Paper paper : allNegativePapers) {
			if (paper.getNumOfPages() < MIN_ACCEPTED_PAPER_PAGE_NUMBER) {
				continue;
			}

			negativePapers.add(paper);
		}

		System.out.println("======================" + positivePapers.size());
		for (Paper paper : positivePapers) {
			double maxRank = AuthorRanking.getInstance().getInstanceValues(
					paper)[0];
			if (!Double.isNaN(maxRank)) {
				System.out.println(maxRank);
			}
		}

		System.out.println("======================" + negativePapers.size());

		for (Paper paper : negativePapers) {
			double maxRank = AuthorRanking.getInstance().getInstanceValues(
					paper)[0];
			if (!Double.isNaN(maxRank)) {
				System.out.println(maxRank);
			}
		}
	}
}
