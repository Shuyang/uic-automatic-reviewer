package edu.uic.cs.automatic_reviewer.feature.term;

import edu.uic.cs.automatic_reviewer.input.Paper;

public class AbstractTFIDF extends TermsTFIDF {

	@Override
	public String getName() {
		return "ABSTR_TFIDF";
	}

	@Override
	protected String getTermsString(Paper paper) {
		return paper.getAbstract();
	}

}
