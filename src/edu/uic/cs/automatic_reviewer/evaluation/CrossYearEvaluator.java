package edu.uic.cs.automatic_reviewer.evaluation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.functions.LibSVM;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.SelectedTag;
import weka.core.Utils;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.Normalize;
import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.feature.FeatureDefinition;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfFiguresPerPage;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfFormulasPerPage;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfTablesPerPage;
import edu.uic.cs.automatic_reviewer.feature.ranking.AuthorRanking2;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceComplexity;
import edu.uic.cs.automatic_reviewer.feature.term.AbstractTFIDF;
import edu.uic.cs.automatic_reviewer.feature.term.FashionTechniques;
import edu.uic.cs.automatic_reviewer.feature.term.TitleTFIDF;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution.Year;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class CrossYearEvaluator implements Constants.Evaluation {

	private static final Year YEAR_FOR_TRAINING = Year._2011;

	private static final Year YEAR_FOR_TESTING = Year._2012;

	private static Feature[] gatherAllFeatures(Year year) {

		return new Feature[] {
				// features
				new NumberOfFiguresPerPage(), //
				new NumberOfFormulasPerPage(), //
				new NumberOfTablesPerPage(), //
				new TitleTFIDF(), //
				new AbstractTFIDF(), //
				new TopicDistribution(year), //
				new FashionTechniques(), //
				AuthorRanking2.getInstance(), //
				new SentenceComplexity(), //
		};
	}

	public static void main(String[] args) throws Exception {

		Map<String, double[]> resultByFeatures = new TreeMap<String, double[]>();

		List<List<Feature>> powerset_training = powerset(gatherAllFeatures(YEAR_FOR_TRAINING));
		List<List<Feature>> powerset_testing = powerset(gatherAllFeatures(YEAR_FOR_TESTING));
		Assert.isTrue(powerset_training.size() == powerset_testing.size());

		for (int index = 0; index < powerset_training.size(); index++) {
			List<Feature> features_training = powerset_training.get(index);
			List<Feature> features_testing = powerset_testing.get(index);

			StringBuilder featureNamesTraining = new StringBuilder();
			for (Feature feature_training : features_training) {
				featureNamesTraining.append(feature_training.getName() + " | ");
			}
			StringBuilder featureNamesTesting = new StringBuilder();
			for (Feature feature_testing : features_testing) {
				featureNamesTesting.append(feature_testing.getName() + " | ");
			}
			Assert.isTrue(featureNamesTraining.toString().equals(
					featureNamesTesting.toString()));

			System.out.println("Using " + featureNamesTraining);

			Instances trainingData = createDataset(features_training,
					YEAR_FOR_TRAINING);
			Classifier classifier = getClassifier();
			classifier.buildClassifier(trainingData);

			Instances testingData = createDataset(features_testing,
					YEAR_FOR_TESTING);

			double[] result = runEvaluation(classifier, trainingData,
					testingData);

			resultByFeatures.put(featureNamesTraining.toString(), result);

			System.out
					.println(featureNamesTraining.toString()
							+ "\t"
							+ (index + 1)
							+ "/"
							+ powerset_training.size()
							+ "=================================================================\n");
		}

		System.out
				.println("=================================================================\n");
		for (Entry<String, double[]> entry : resultByFeatures.entrySet()) {
			double[] result = entry.getValue();
			System.out.printf(
					"P: %.4f\tR: %.4f\tF: %.4f\tC: %.4f\tI: %.4f\t%s\n",
					result[0], result[1], result[2], result[3], result[4],
					entry.getKey());
		}

	}

	private static double[] runEvaluation(Classifier classifier,
			Instances trainingData, Instances testingData) throws Exception {

		Evaluation evaluation = new Evaluation(trainingData);
		evaluation.evaluateModel(classifier, testingData);

		// output evaluation
		System.out.println();
		System.out.println("=== Setup ===");
		System.out.println("Classifier: " + classifier.getClass().getName()
				+ " " + Utils.joinOptions(classifier.getOptions()));
		System.out.println();
		System.out.println(evaluation.toSummaryString("=== Using "
				+ YEAR_FOR_TRAINING + " to predict " + YEAR_FOR_TESTING
				+ " ===", true));
		System.out.println(evaluation.toMatrixString());
		double precision = evaluation.weightedPrecision();
		double recall = evaluation.weightedRecall();
		double fMeasure = evaluation.weightedFMeasure();
		System.out.println("precision: " + precision);
		System.out.println("recall: " + recall);
		System.out.println("fMeasure: " + fMeasure);

		double correct = evaluation.correct();
		double incorrect = evaluation.incorrect();

		return new double[] { precision, recall, fMeasure, correct, incorrect };
	}

	private static Classifier getClassifier() {

		LibSVM classifier = new LibSVM();

		classifier.setSVMType(new SelectedTag(LibSVM.SVMTYPE_NU_SVC,
				LibSVM.TAGS_SVMTYPE));
		// classifier.setKernelType(new
		// SelectedTag(LibSVM.KERNELTYPE_POLYNOMIAL,
		// LibSVM.TAGS_KERNELTYPE));
		// classifier.setNu(0.5);
		// classifier.setCost(500);

		return classifier;
	}

	private static FastVector defineFeatures(List<Feature> featuresToUse) {

		FastVector result = new FastVector();

		for (FeatureDefinition definition : featuresToUse) {
			String[] names = definition.getSubFeatureNames();
			int numOfSubFeatures = definition.getNumberOfSubFeatures();
			Assert.isTrue(numOfSubFeatures >= 1
					&& numOfSubFeatures == names.length);

			for (int index = 0; index < numOfSubFeatures; index++) {
				result.addElement(new Attribute(names[index]));
			}
		}

		// Declare the class attribute along with its values
		FastVector fvClassVal = new FastVector(2);
		fvClassVal.addElement(Boolean.TRUE.toString());
		fvClassVal.addElement(Boolean.FALSE.toString());
		result.addElement(new Attribute("CLASS", fvClassVal));

		return result;
	}

	private static void addInstance(List<Feature> features, Instances dataSet,
			FastVector featureDefs, Paper paper, Boolean theClass) {

		Instance instance = new Instance(featureDefs.size());
		// System.out.println(paper.getMetadata().getPaperFileName());

		int index = 0;
		for (Feature feature : features) {
			// System.out.println(feature.getName());
			for (double value : feature.getInstanceValues(paper)) {
				instance.setValue((Attribute) featureDefs.elementAt(index++),
						value);
			}
		}

		// the final one is the class
		instance.setValue((Attribute) featureDefs.elementAt(index++),
				theClass.toString());
		Assert.isTrue(index == featureDefs.size());

		dataSet.add(instance);
	}

	private static Instances createDataset(List<Feature> features, Year year) {

		List<Paper> positivePapers;
		List<Paper> negativePapers;

		if (year.equals(Year._All)) {
			positivePapers = PaperCache.getInstance().getAllPapers(
					PaperPublishType.LongPaper);
			negativePapers = PaperCache.getInstance().getAllPapers(
					PaperPublishType.WorkshopPaper,
					PaperPublishType.StudentWorkshopPaper);
		} else {
			positivePapers = PaperCache.getInstance().getPapers(
					year.getYears(), PaperPublishType.LongPaper);
			negativePapers = PaperCache.getInstance().getPapers(
					year.getYears(), PaperPublishType.WorkshopPaper,
					PaperPublishType.StudentWorkshopPaper);
		}

		FastVector featureDefs = defineFeatures(features);

		Instances dataSet = new Instances("DATA_SET", featureDefs,
				positivePapers.size() + negativePapers.size());
		// Set class index
		dataSet.setClassIndex(featureDefs.size() - 1);

		// add into dataset
		for (Paper paper : positivePapers) {
			addInstance(features, dataSet, featureDefs, paper, Boolean.TRUE);
		}

		for (Paper paper : negativePapers) {
			if (paper.getNumOfPages() < MIN_ACCEPTED_PAPER_PAGE_NUMBER) {
				continue;
			}
			addInstance(features, dataSet, featureDefs, paper, Boolean.FALSE);
		}

		// normalize [0, 1]
		Normalize normalize = new Normalize();
		// un-comment below for normalize [-1, 1]
		// normalize.setScale(2.0);
		// normalize.setTranslation(-1.0);

		try {
			normalize.setInputFormat(dataSet);
			dataSet = Filter.useFilter(dataSet, normalize);
		} catch (Exception e) {
			throw new AutomaticReviewerException(e);
		}

		return dataSet;
	}

	private static <E> List<List<E>> powerset(E[] list/* , boolean needEmptySet */) {
		List<List<E>> ps = new ArrayList<List<E>>();
		ps.add(new ArrayList<E>()); // add the empty set

		// for every item in the original list
		for (E item : list) {
			List<List<E>> newPs = new ArrayList<List<E>>();

			for (List<E> subset : ps) {
				// copy all of the current powerset's subsets
				newPs.add(subset);

				// plus the subsets appended with the current item
				List<E> newSubset = new ArrayList<E>(subset);
				newSubset.add(item);
				newPs.add(newSubset);
			}

			// powerset is now powerset of list.subList(0, list.indexOf(item)+1)
			ps = newPs;
		}

		/* if (!needEmptySet) { */
		ps.remove(0);
		/* } */

		return ps;
	}

}
