package edu.uic.cs.automatic_reviewer.prediction;

import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class TopicDistribution4Prediction extends TopicDistribution {

	public TopicDistribution4Prediction(Year year) {
		super(year);
	}

	@Override
	public double[] getInstanceValues(Paper paper) {
		return predictTopicDistribution(paper);
	}
}
