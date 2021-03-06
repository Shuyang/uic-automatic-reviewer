package edu.uic.cs.automatic_reviewer.feature.term;

import edu.uic.cs.automatic_reviewer.input.Paper;

public class TitleTFIDF extends TermsTFIDF {

	@Override
	public String getName() {
		return "TITLE_TFIDF";
	}

	@Override
	protected String getTermsString(Paper paper) {
		return paper.getTitle();
	}

}
