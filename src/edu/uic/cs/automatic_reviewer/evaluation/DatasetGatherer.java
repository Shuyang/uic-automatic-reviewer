package edu.uic.cs.automatic_reviewer.evaluation;

import java.io.File;
import java.util.List;

import org.apache.commons.io.FileUtils;

import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
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
import edu.uic.cs.automatic_reviewer.feature.topic.TopicDistribution.Year;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.input.PaperPublishType;
import edu.uic.cs.automatic_reviewer.misc.Assert;

public class DatasetGatherer {

	private static final Year YEAR = Year._2011;

	private static final Feature[] FEATURES = gatherAllFeatures();

	private static final int MIN_ACCEPTED_PAPER_PAGE_NUMBER = 8;

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

		Assert.isTrue(false,
				"Comment this line if you want to gather dataset into files! ");

		Instances data = createDataset();

		String arffContent = data.toString();
		String outputFileName = getOutputFileName();
		FileUtils.writeStringToFile(new File(outputFileName), arffContent);

		System.out.println("Output dataset to: " + outputFileName);
	}

	private static String getOutputFileName() {
		StringBuilder sb = new StringBuilder("dataset/dataset");
		sb.append(YEAR.name()).append(".arff");
		return sb.toString();
	}

	// DO NOT MODIFIY THE CODE BELOW, UNLESS YOU KNOW WHAT EXACTLY THEY ARE!
	// /////////////////////////////////////////////////////////////////////////
	// /////////////////////////////////////////////////////////////////////////
	private static FastVector defineFeatures() {

		FastVector result = new FastVector();

		for (FeatureDefinition definition : FEATURES) {
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

		List<Paper> positivePapers;
		List<Paper> negativePapers;

		if (YEAR.equals(Year._All)) {
			positivePapers = PaperCache.getInstance().getAllPapers(
					PaperPublishType.LongPaper);
			negativePapers = PaperCache.getInstance().getAllPapers(
					PaperPublishType.WorkshopPaper,
					PaperPublishType.StudentWorkshopPaper);
		} else {
			positivePapers = PaperCache.getInstance().getPapers(YEAR.getYear(),
					PaperPublishType.LongPaper);
			negativePapers = PaperCache.getInstance().getPapers(YEAR.getYear(),
					PaperPublishType.WorkshopPaper,
					PaperPublishType.StudentWorkshopPaper);
		}

		FastVector featureDefs = defineFeatures();

		Instances dataSet = new Instances("DATA_SET" + YEAR.name(),
				featureDefs, positivePapers.size() + negativePapers.size());
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

}
