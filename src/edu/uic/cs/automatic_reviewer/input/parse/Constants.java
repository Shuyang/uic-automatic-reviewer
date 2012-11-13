package edu.uic.cs.automatic_reviewer.input.parse;

public interface Constants {

	int PARAGRAPH_MIN_CHAR_LENGTH = 300;

	int REFERENCE_MIN_CHAR_LENGTH = 30;

	double NONALPHA_THRESHOLD_RATIO = 0.3;

	int TITLE_MIN_TERMS_NUMBER = 3;

	// int AUTHOR_INFO_MAX_LINE_NUMBER = 10;

	int MAX_NUMBER_OF_MATCHED_PAPER = 3;

	String ILLEGAL_AUTHOR_NAME_IN_LOW_CASE = "association for computational linguistics";
}
