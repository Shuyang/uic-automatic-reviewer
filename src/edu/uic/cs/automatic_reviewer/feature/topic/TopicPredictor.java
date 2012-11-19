package edu.uic.cs.automatic_reviewer.feature.topic;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;

import cc.mallet.pipe.CharSequence2TokenSequence;
import cc.mallet.pipe.CharSequenceLowercase;
import cc.mallet.pipe.Pipe;
import cc.mallet.pipe.SerialPipes;
import cc.mallet.pipe.TokenSequence2FeatureSequence;
import cc.mallet.pipe.TokenSequenceRemoveStopwords;
import cc.mallet.topics.ParallelTopicModel;
import cc.mallet.topics.TopicInferencer;
import cc.mallet.types.Instance;
import cc.mallet.types.InstanceList;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

public class TopicPredictor implements Constants.Topic {

	private static final Logger LOGGER = LogHelper
			.getLogger(TopicPredictor.class);

	private static final String ADDITIONAL_STOP_WORDS_FILE_NAME = "additional_stop_words.txt";
	private static final Pipe PIPE = initializePipes();

	private static Pipe initializePipes() {

		ArrayList<Pipe> pipeList = new ArrayList<Pipe>();

		// Pipes: lowercase, tokenize, remove stopwords, map to features
		pipeList.add(new CharSequenceLowercase());
		pipeList.add(new CharSequence2TokenSequence(
				"\\p{L}[\\p{L}\\p{P}]+\\p{L}"));

		TokenSequenceRemoveStopwords removeStopwordsPipe = new TokenSequenceRemoveStopwords();
		String[] additionalStopWords = loadAdditionalStopWords();
		removeStopwordsPipe.addStopWords(additionalStopWords);
		pipeList.add(removeStopwordsPipe);

		pipeList.add(new TokenSequence2FeatureSequence());

		return new SerialPipes(pipeList);
	}

	private static String[] loadAdditionalStopWords() {

		InputStream input = TopicPredictor.class
				.getResourceAsStream(ADDITIONAL_STOP_WORDS_FILE_NAME);
		try {
			@SuppressWarnings("unchecked")
			List<String> lines = IOUtils.readLines(input);

			List<String> result = new ArrayList<String>();
			for (String line : lines) {
				String[] words = line.split("\\s+");
				for (int i = 0; i < words.length; i++) {
					result.add(words[i]);
				}
			}

			LOGGER.info("Loaded additional stopwords " + result);

			return result.toArray(new String[result.size()]);

		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		} finally {
			IOUtils.closeQuietly(input);
		}
	}

	private static class PaperInstanceIterator implements Iterator<Instance> {

		private Iterator<Paper> paperIterator;

		private PaperInstanceIterator(List<Paper> allPapers) {
			this.paperIterator = allPapers.iterator();
		}

		@Override
		public boolean hasNext() {
			return paperIterator.hasNext();
		}

		@Override
		public Instance next() {
			Paper paper = paperIterator.next();
			String content = getPaperContent(paper);

			return new Instance(content, null, paper.getMetadata()
					.getPaperFileName(), null);
		}

		@Override
		public void remove() {
			// paperIterator.remove();
			throw new UnsupportedOperationException();
		}

	}

	private static String getPaperContent(Paper paper) {
		StringBuilder content = new StringBuilder();

		content.append(paper.getAbstract());
		for (String paragraph : paper.getContentParagraphs()) {
			content.append(paragraph);
		}

		return content.toString();
	}

	public ParallelTopicModel trainTopicModel(List<Paper> allPapers,
			int numberOfTopics) {

		InstanceList instances = new InstanceList(PIPE);
		instances.addThruPipe(new PaperInstanceIterator(allPapers));

		ParallelTopicModel model = new ParallelTopicModel(numberOfTopics,
				LDA_TRAINING_ALPHA_SUM, LDA_TRAINING_BETA);
		model.addInstances(instances);

		model.setNumThreads(LDA_TRAINING_NUMBER_OF_THREADS);
		model.setNumIterations(LDA_TRAINING_NUMBER_OF_ITERATIONS);

		try {
			model.estimate();
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}

		return model;
	}

	public double[] predictPaper(Paper paper, ParallelTopicModel model) {
		TopicInferencer inferencer = model.getInferencer();
		String content = getPaperContent(paper);

		InstanceList instances = new InstanceList(PIPE);
		instances.addThruPipe(new Instance(content, null, paper.getMetadata()
				.getPaperFileName(), null));
		return inferencer.getSampledDistribution(instances.get(0),
				LDA_PREDICTING_NUMBER_OF_ITERATIONS, LDA_PREDICTING_THINNING,
				LDA_PREDICTING_BURN_IN);
	}

	public static void main(String[] args) {

		List<Paper> papers = PaperCache.getInstance().getPapers(2012);
		TopicPredictor predictor = new TopicPredictor();
		ParallelTopicModel model = predictor.trainTopicModel(papers,
				NUMBER_OF_TOPICS);

		System.out.println();

		double[] result = predictor.predictPaper(PaperCache.getInstance()
				.getPapers(2007).get(2), model);
		int index = 1;
		for (double score : result) {
			System.out.println(index++ + "\t" + score);
		}
	}

}
