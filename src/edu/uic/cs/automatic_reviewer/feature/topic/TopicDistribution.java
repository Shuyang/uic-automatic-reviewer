package edu.uic.cs.automatic_reviewer.feature.topic;

import java.util.List;

import org.apache.log4j.Logger;

import cc.mallet.topics.ParallelTopicModel;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;

public class TopicDistribution implements Feature {

	private static final Logger LOGGER = LogHelper
			.getLogger(TopicDistribution.class);

	private static final String MODEL_CACHE_FILE = "data/lda_model.cache";

	private TopicPredictor topicPredictor = new TopicPredictor();
	private ParallelTopicModel model;

	public TopicDistribution(List<Paper> papersToTrainModel) {

		if (model == null) {
			synchronized (this) {
				if (model == null) {
					model = (ParallelTopicModel) SerializationHelper
							.deserialize(MODEL_CACHE_FILE);
					if (model == null) {
						trainAndCacheTopicModel(papersToTrainModel);
					}
				}
			}
		}

		Assert.notNull(model);
	}

	private void trainAndCacheTopicModel(List<Paper> papersToTrainModel) {
		LOGGER.info(LogHelper.LOG_LAYER_ONE_BEGIN + "Train topic model using "
				+ papersToTrainModel.size() + " papers...");
		model = topicPredictor.trainTopicModel(papersToTrainModel,
				TopicPredictor.NUMBER_OF_TOPICS);
		LOGGER.info(LogHelper.LOG_LAYER_ONE_END + "Train topic model... Done.");

		LOGGER.info(LogHelper.LOG_LAYER_ONE_BEGIN + "Caching topic model...");
		SerializationHelper.serialize(model, MODEL_CACHE_FILE);
		LOGGER.info(LogHelper.LOG_LAYER_ONE_END
				+ "Caching topic model... Done.");
	}

	@Override
	public String getName() {
		return "LDA_TOPIC";
	}

	@Override
	public int getNumberOfSubFeatures() {
		return TopicPredictor.NUMBER_OF_TOPICS;
	}

	@Override
	public double[] getInstanceValues(Paper paper) {
		return topicPredictor.predictPaper(paper, model);
	}
}
