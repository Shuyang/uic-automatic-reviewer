package edu.uic.cs.automatic_reviewer.prediction;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import weka.classifiers.Classifier;
import weka.classifiers.functions.LibSVM;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.SelectedTag;
import weka.core.converters.ArffLoader;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfFormulasPerPage;
import edu.uic.cs.automatic_reviewer.feature.metadata.NumberOfTablesPerPage;
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution.Year;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.parse.PaperParser;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class ACLAcceptancePredictor {

	private static final String FILE_NAME = "dataset_replaced_missing_normalized_best_2012.arff";

	private static final Feature[] FEATURES = gatherAllFeatures();

	private Instances instances;

	private PaperParser paperParser = new PaperParser();

	private static Feature[] gatherAllFeatures() {

		return new Feature[] {
				// features
				new NumberOfFormulasPerPage(), //
				new NumberOfTablesPerPage(), //
				new TopicDistribution4Prediction(Year._2012), //
				new SentenceComplexity4Prediction() //
		};
	}

	public ACLAcceptancePredictor() {
		ArffLoader arffLoader = new ArffLoader();
		try {
			arffLoader.setSource(ACLAcceptancePredictor.class
					.getResourceAsStream(FILE_NAME));
			instances = arffLoader.getDataSet();
			instances.setClassIndex(instances.numAttributes() - 1);

			// System.out.println(instances);
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}
	}

	public boolean predict(String paperFile) {
		Paper paper = paperParser.parse(paperFile);
		return predict(paper);
	}

	private boolean predict(Paper paper) {
		Classifier classifier = getClassifier();
		FastVector featureDefs = defineFeatures();

		Instance instance = createInstance(paper, featureDefs);
		instance.setDataset(instances);

		Attribute classAttribute = instance.classAttribute();
		@SuppressWarnings("unchecked")
		Enumeration<String> enu = classAttribute.enumerateValues();
		List<Boolean> theClasses = new ArrayList<Boolean>();
		while (enu.hasMoreElements()) {
			Boolean theClasse = Boolean.parseBoolean(enu.nextElement());
			theClasses.add(theClasse);
		}

		try {
			classifier.buildClassifier(instances);
			int resultIndex = (int) classifier.classifyInstance(instance);
			// System.out.println("********" + resultIndex);
			return theClasses.get(resultIndex);

		} catch (Exception e) {
			throw new AutomaticReviewerException(e);
		}
	}

	private Instance createInstance(Paper paper, FastVector featureDefs) {
		Instance instance = new Instance(featureDefs.size());

		int index = 0;
		for (Feature feature : FEATURES) {
			for (double value : feature.getInstanceValues(paper)) {
				instance.setValue((Attribute) featureDefs.elementAt(index++),
						value);
			}
		}

		return instance;
	}

	public static void main(String[] args) {
		ACLAcceptancePredictor predictor = new ACLAcceptancePredictor();
		String paperFile = "D:/Data/UIC Documents/Computer Science/CS_521/Project/paper/final/automatic_reviewer.pdf";
		boolean result = predictor.predict(paperFile);

		System.out.println(result);
	}

	private FastVector defineFeatures() {
		FastVector result = new FastVector();
		@SuppressWarnings("unchecked")
		Enumeration<Attribute> enumeration = instances.enumerateAttributes();
		while (enumeration.hasMoreElements()) {
			Attribute attribute = enumeration.nextElement();
			result.addElement(attribute);
			// System.out.println(attribute);
		}

		Attribute attribute = instances
				.attribute(instances.numAttributes() - 1);
		result.addElement(attribute);
		// System.out.println(attribute);

		return result;
	}

	private Classifier getClassifier() {
		LibSVM classifier = new LibSVM();
		classifier.setSVMType(new SelectedTag(LibSVM.SVMTYPE_NU_SVC,
				LibSVM.TAGS_SVMTYPE));
		return classifier;
	}
}
