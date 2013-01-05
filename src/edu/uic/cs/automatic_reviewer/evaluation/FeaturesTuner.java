package edu.uic.cs.automatic_reviewer.evaluation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.TreeMap;

import weka.classifiers.Classifier;
import weka.classifiers.functions.LibSVM;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.SelectedTag;
import weka.core.Utils;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.feature.FeatureDefinition;
import edu.uic.cs.automatic_reviewer.feature.InstanceCreator;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfFiguresPerPage;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfFormulasPerPage;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfTablesPerPage;
import edu.uic.cs.automatic_reviewer.feature.ranking.AuthorRanking;
import edu.uic.cs.automatic_reviewer.feature.sentence.SentenceComplexity;
import edu.uic.cs.automatic_reviewer.feature.term.AbstractTFIDF;
import edu.uic.cs.automatic_reviewer.feature.term.FashionTechniques;
import edu.uic.cs.automatic_reviewer.feature.term.TitleTFIDF;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class FeaturesTuner {

	private static final int MIN_ACCEPTED_PAPER_PAGE_NUMBER = 8;

	private static final int YEAR = 2012;

	private static Feature[] gatherAllFeatures() {

		return new Feature[] {
				// features
				new NumberOfFiguresPerPage(), //
				new NumberOfFormulasPerPage(), //
				new NumberOfTablesPerPage(), //
				new TitleTFIDF(), //
				new AbstractTFIDF(), //
				new TopicDistribution(YEAR), //
				new FashionTechniques(), //
				AuthorRanking.getInstance(), //
				new SentenceComplexity() //
		};
	}

	public static void main(String[] args) throws Exception {

		Map<String, double[]> resultByFeatures = new TreeMap<String, double[]>();

		List<List<Feature>> powerset = powerset(gatherAllFeatures());
		for (List<Feature> features : powerset) {
			StringBuilder featureNames = new StringBuilder();
			for (Feature feature : features) {
				featureNames.append(feature.getName() + " | ");
			}

			System.out.println("Using " + featureNames);
			Instances data = createDataset(features);
			Classifier classifier = getClassifier(data);
			double[] result = runEvaluation(classifier, data);

			resultByFeatures.put(featureNames.toString(), result);
		}

		System.out.println("===============================================\n");
		for (Entry<String, double[]> entry : resultByFeatures.entrySet()) {
			double[] result = entry.getValue();
			System.out.printf("P: %.4f, R: %.4f, F: %.4f >>>>> %s\n",
					result[0], result[1], result[2], entry.getKey());
		}

	}

	private static double[] runEvaluation(Classifier classifier,
			Instances dataset) throws Exception {

		int seed = 1; // randomize data
		int folds = 10;

		// perform cross-validation
		weka.classifiers.Evaluation evaluation = new weka.classifiers.Evaluation(
				dataset);
		evaluation.crossValidateModel(classifier, dataset, folds, new Random(
				seed));

		// output evaluation
		System.out.println();
		System.out.println("=== Setup ===");
		System.out.println("Classifier: " + classifier.getClass().getName()
				+ " " + Utils.joinOptions(classifier.getOptions()));
		System.out.println();
		System.out.println(evaluation.toSummaryString("=== " + folds
				+ "-fold Cross-validation ===", true));
		System.out.println(evaluation.toMatrixString());
		double precision = evaluation.precision(0);
		double recall = evaluation.recall(0);
		double fMeasure = evaluation.fMeasure(0);
		System.out.println("precision: " + precision);
		System.out.println("recall: " + recall);
		System.out.println("fMeasure: " + fMeasure);

		return new double[] { precision, recall, fMeasure };
	}

	private static Classifier getClassifier(Instances data) {

		LibSVM classifier = new LibSVM();

		classifier.setSVMType(new SelectedTag(LibSVM.SVMTYPE_NU_SVC,
				LibSVM.TAGS_SVMTYPE));
		classifier.setKernelType(new SelectedTag(LibSVM.KERNELTYPE_RBF,
				LibSVM.TAGS_KERNELTYPE));
		classifier.setDegree(3);
		classifier.setGamma(1.0 / (data.numAttributes() - 1));
		classifier.setCoef0(0);
		classifier.setNu(0.5);
		classifier.setCacheSize(5000);
		classifier.setCost(500);
		classifier.setEps(1e-3);
		classifier.setLoss(0.1);
		classifier.setShrinking(true);
		classifier.setProbabilityEstimates(false);
		classifier.setWeights("");

		return classifier;
	}

	private static FastVector defineFeatures(List<Feature> featuresToUse) {

		FastVector result = new FastVector();

		for (FeatureDefinition definition : featuresToUse) {
			String name = definition.getName();
			int numOfSubFeatures = definition.getNumberOfSubFeatures();
			Assert.isTrue(numOfSubFeatures >= 1);

			if (numOfSubFeatures == 1) {
				result.addElement(new Attribute(name));
			} else {
				for (int index = 0; index < numOfSubFeatures; index++) {
					result.addElement(new Attribute(name + "_" + index));
				}
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

		int index = 0;
		for (InstanceCreator definition : features) {
			for (double value : definition.getInstanceValues(paper)) {
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

	private static Instances createDataset(List<Feature> features) {

		List<Paper> positivePapers = PaperCache.getInstance().getPapers(YEAR,
				PaperPublishType.LongPaper);
		List<Paper> negativePapers = PaperCache.getInstance().getPapers(YEAR,
				PaperPublishType.WorkshopPaper,
				PaperPublishType.StudentWorkshopPaper);

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
