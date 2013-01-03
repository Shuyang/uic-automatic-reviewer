package edu.uic.cs.automatic_reviewer.evaluation;

import java.util.List;
import java.util.Random;

import weka.classifiers.Classifier;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.functions.LibSVM;
import weka.classifiers.trees.J48;
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

	private static Feature[] gatherAllFeatures() {

		List<Paper> papersToTrainLDAModel = PaperCache.getInstance()
				.getAllPapers();

		return new Feature[] { new NumberOfFiguresPerPage(),
				new NumberOfFormulasPerPage(), new NumberOfTablesPerPage(),
				/*new TitleTFIDF(), new TopicDistribution(papersToTrainLDAModel),
				new FashionTechniques(), AuthorRanking.getInstance(),
				new SentenceComplexity() */};
	}

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
			Paper paper, Boolean isPositive) {

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
				isPositive.toString());
		Assert.isTrue(index == featureDefs.size());

		dataSet.add(instance);
	}

	private static Instances createDataset(int year) {

		List<Paper> positivePapers = PaperCache.getInstance().getPapers(year,
				PaperPublishType.LongPaper);
		List<Paper> negativePapers = PaperCache.getInstance().getPapers(year,
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

	private static void runEvaluation(Classifier classifier, Instances data)
			throws Exception {
		// randomize data
		int seed = 10;
		int folds = 10;

		Random rand = new Random(seed);
		Instances randData = new Instances(data);
//		randData.randomize(rand);
		if (randData.classAttribute().isNominal()) {
			randData.stratify(folds);
		}

		// perform cross-validation
		weka.classifiers.Evaluation eval = new weka.classifiers.Evaluation(
				randData);

		for (int n = 0; n < folds; n++) {
			Instances train = randData.trainCV(folds, n);
			Instances test = randData.testCV(folds, n);

			// build and evaluate classifier
			Classifier clsCopy = Classifier.makeCopy(classifier);
			clsCopy.buildClassifier(train);
			eval.evaluateModel(clsCopy, test);
		}

		// output evaluation
		System.out.println();
		System.out.println("=== Setup ===");
		System.out.println("Classifier: " + classifier.getClass().getName()
				+ " " + Utils.joinOptions(classifier.getOptions()));
		System.out.println("Dataset: " + data.relationName());
		System.out.println("Folds: " + folds);
		System.out.println("Seed: " + seed);
		System.out.println();
		System.out.println(eval.toSummaryString("=== " + folds
				+ "-fold Cross-validation ===", true));
	}

	public static void main(String[] args) throws Exception {

		Instances data = createDataset(2012);

		System.out.println("=================================================");
		for (int index = 0; index < data.numInstances(); index++) {
			System.out.println(index + "\t" + data.instance(index));
		}
		System.out.println("=================================================");

		Classifier classifier = /* new J48(); */getClassifier(data);
		runEvaluation(classifier, data);
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
}
