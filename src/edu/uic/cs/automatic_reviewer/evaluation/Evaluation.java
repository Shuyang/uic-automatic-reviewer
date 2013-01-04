package edu.uic.cs.automatic_reviewer.evaluation;

import java.util.List;
import java.util.Random;

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
import edu.uic.cs.automatic_reviewer.feature.term.FashionTechniques;
import edu.uic.cs.automatic_reviewer.feature.term.TitleTFIDF;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class Evaluation {

	private static final Feature[] FEATURES = gatherAllFeatures();

	private static final int MIN_ACCEPTED_PAPER_PAGE_NUMBER = 8;

	private static final int YEAR = 2012;

	private static Feature[] gatherAllFeatures() {

		return new Feature[] {
				// features
				new NumberOfFiguresPerPage(), //
				new NumberOfFormulasPerPage(), //
				new NumberOfTablesPerPage(), //
				new TitleTFIDF(), //
				new TopicDistribution(YEAR), //
				new FashionTechniques(), //
				AuthorRanking.getInstance(), //
				new SentenceComplexity() //
		};
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

	public static void main(String[] args) throws Exception {

		Instances data = createDataset();

		System.out.println("=================================================");
		for (int index = 0; index < data.numInstances(); index++) {
			System.out.println(index + "\t" + data.instance(index));
		}
		System.out.println("=================================================");
		System.out.println(data.toString());
		System.out.println(data.toSummaryString());

		Classifier classifier = getClassifier(data);
		runEvaluation(classifier, data);
	}

	// DO NOT MODIFIY THE CODE BELOW, UNLESS YOU KNOW WHAT EXACTLY THEY ARE!
	// /////////////////////////////////////////////////////////////////////////
	// /////////////////////////////////////////////////////////////////////////
	private static FastVector defineFeatures() {

		FastVector features = new FastVector();

		for (FeatureDefinition definition : FEATURES) {
			String name = definition.getName();
			int numOfSubFeatures = definition.getNumberOfSubFeatures();
			Assert.isTrue(numOfSubFeatures >= 1);

			if (numOfSubFeatures == 1) {
				features.addElement(new Attribute(name));
			} else {
				for (int index = 0; index < numOfSubFeatures; index++) {
					features.addElement(new Attribute(name + "_" + index));
				}
			}
		}

		// Declare the class attribute along with its values
		FastVector fvClassVal = new FastVector(2);
		fvClassVal.addElement(Boolean.TRUE.toString());
		fvClassVal.addElement(Boolean.FALSE.toString());
		features.addElement(new Attribute("CLASS", fvClassVal));

		return features;
	}

	private static void addInstance(Instances dataSet, FastVector featureDefs,
			Paper paper, Boolean theClass) {

		Instance instance = new Instance(featureDefs.size());

		int index = 0;
		for (InstanceCreator definition : FEATURES) {
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

	private static Instances createDataset() {

		List<Paper> positivePapers = PaperCache.getInstance().getPapers(YEAR,
				PaperPublishType.LongPaper);
		List<Paper> negativePapers = PaperCache.getInstance().getPapers(YEAR,
				PaperPublishType.WorkshopPaper,
				PaperPublishType.StudentWorkshopPaper);

		FastVector featureDefs = defineFeatures();

		Instances dataSet = new Instances("DATA_SET", featureDefs,
				positivePapers.size() + negativePapers.size());
		// Set class index
		dataSet.setClassIndex(featureDefs.size() - 1);

		// add into dataset
		for (Paper paper : positivePapers) {
			addInstance(dataSet, featureDefs, paper, Boolean.TRUE);
		}

		for (Paper paper : negativePapers) {
			if (paper.getNumOfPages() < MIN_ACCEPTED_PAPER_PAGE_NUMBER) {
				continue;
			}
			addInstance(dataSet, featureDefs, paper, Boolean.FALSE);
		}

		return dataSet;
	}

	private static void runEvaluation(Classifier classifier, Instances dataset)
			throws Exception {

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
		System.out.println("fMeasure" + fMeasure);
	}
}
