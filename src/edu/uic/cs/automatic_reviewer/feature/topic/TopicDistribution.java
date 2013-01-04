package edu.uic.cs.automatic_reviewer.feature.topic;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import cc.mallet.topics.ParallelTopicModel;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;

public class TopicDistribution implements Feature {

	private static final Logger LOGGER = LogHelper
			.getLogger(TopicDistribution.class);

	private static final String _MODEL_CACHE_FILE = "data/lda_model_$YEAR$.cache";
	private static final String _PAPER_TOPIC_DISTRIBUTION_CACHE_FILE = "data/paper_topic_distribution_$YEAR$.cache";

	private TopicPredictor topicPredictor = new TopicPredictor();

	private ParallelTopicModel model;
	private String modelCacheFile;

	private Map<String, double[]> paperTopicDistribution;
	private String paperTopicDistributionCacheFile;

	private int year;

	public TopicDistribution(int year) {
		this.year = year;
		modelCacheFile = _MODEL_CACHE_FILE.replace("$YEAR$", "" + year);
		paperTopicDistributionCacheFile = _PAPER_TOPIC_DISTRIBUTION_CACHE_FILE
				.replace("$YEAR$", "" + year);

		if (model == null) {
			synchronized (this) {
				if (model == null) {
					model = (ParallelTopicModel) SerializationHelper
							.deserialize(modelCacheFile);
					if (model == null) {
						trainAndCacheTopicModel();
					}
				}
			}
		}

		Assert.notNull(model);
	}

	private void trainAndCacheTopicModel() {

		List<Paper> papersToTrainModel = PaperCache.getInstance().getPapers(
				year);

		LOGGER.info(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Train topic model using papers of year [" + year + "]...");
		model = topicPredictor.trainTopicModel(papersToTrainModel,
				TopicPredictor.NUMBER_OF_TOPICS);
		LOGGER.info(LogHelper.LOG_LAYER_ONE_END
				+ "Train topic model using papers of year [" + year
				+ "]... Done.");

		LOGGER.info(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Caching topic model of year [" + year + "]...");
		SerializationHelper.serialize(model, modelCacheFile);
		LOGGER.info(LogHelper.LOG_LAYER_ONE_END
				+ "Caching topic model of year [" + year + "]... Done.");
	}

	@Override
	public String getName() {
		return "LDA_TOPIC";
	}

	@Override
	public int getNumberOfSubFeatures() {
		return TopicPredictor.NUMBER_OF_TOPICS;
	}

	@SuppressWarnings("unchecked")
	@Override
	public double[] getInstanceValues(Paper paper) {

		if (paperTopicDistribution == null) {
			synchronized (this) {
				if (paperTopicDistribution == null) {
					paperTopicDistribution = (Map<String, double[]>) SerializationHelper
							.deserialize(paperTopicDistributionCacheFile);
					if (paperTopicDistribution == null) {
						cachePaperTopicDistribution();
					}
				}
			}
		}

		Assert.notNull(paperTopicDistribution);
		return paperTopicDistribution.get(paper.getMetadata()
				.getPaperFileName());
	}

	private void cachePaperTopicDistribution() {
		List<Paper> papers = PaperCache.getInstance().getPapers(year);

		LOGGER.info(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "cache topic distribution of papers of year [" + year
				+ "]...");

		paperTopicDistribution = new HashMap<String, double[]>();
		for (Paper paper : papers) {
			double[] topic = topicPredictor.predictPaper(paper, model);
			paperTopicDistribution.put(paper.getMetadata().getPaperFileName(),
					topic);

			if (LOGGER.isInfoEnabled()) {
				LOGGER.info(paper.getMetadata().getPaperFileName() + "\t"
						+ Arrays.toString(topic));
			}
		}

		SerializationHelper.serialize(paperTopicDistribution,
				paperTopicDistributionCacheFile);

		LOGGER.info(LogHelper.LOG_LAYER_ONE_END
				+ "cache topic distribution of papers of year [" + year
				+ "]... Done.");
	}
}
