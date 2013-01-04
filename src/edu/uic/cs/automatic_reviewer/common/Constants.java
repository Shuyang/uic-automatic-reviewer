package edu.uic.cs.automatic_reviewer.common;

public interface Constants {

	interface InputParse {

		int PARAGRAPH_MIN_CHAR_LENGTH = 300;

		int REFERENCE_MIN_CHAR_LENGTH = 30;

		double NONALPHA_THRESHOLD_RATIO = 0.3;

		int TITLE_MIN_TERMS_NUMBER = 3;

		// int AUTHOR_INFO_MAX_LINE_NUMBER = 10;

		int MAX_NUMBER_OF_MATCHED_PAPER = 3;

		String ILLEGAL_AUTHOR_NAME_IN_LOW_CASE = "association for computational linguistics";
	}

	interface Ranking {

		/**
		 * Number of author rankings we want to store
		 */
		int NUMBER_OF_AUTHORS_TO_RETRIEVE = 2000;

		int NO_RANK_VALUE = Integer.MAX_VALUE;

		/**
		 * Lucene search score threshold, only result has score above or equal
		 * to this score will be considered as matched result and be returned.
		 */
		float SCORE_THRESHOLD = 3.5f;
	}

	interface Feature {

		int MIN_FREQUENCY_OF_TERMS = 3;

		int MAX_NUMBER_OF_PAGES_PER_PAPER = 10;
	}

	interface Topic {

		int NUMBER_OF_TOPICS = 20;

		double LDA_TRAINING_ALPHA_SUM = 1.0;
		double LDA_TRAINING_BETA = 0.01;
		int LDA_TRAINING_NUMBER_OF_THREADS = 4;
		// for real applications, use 1000 to 2000 iterations)
		int LDA_TRAINING_NUMBER_OF_ITERATIONS = 2000;

		// TODO I am not sure about these three parameters
		int LDA_PREDICTING_THINNING = 5;
		int LDA_PREDICTING_BURN_IN = 100;
		int LDA_PREDICTING_NUMBER_OF_ITERATIONS = 2000;

	}

	interface SentenceComplexity {
		int MAX_COMPLEXITY = 40;
	}
}
