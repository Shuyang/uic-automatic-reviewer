package edu.uic.cs.automatic_reviewer.evaluation.sentence;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.math3.stat.descriptive.moment.Variance;

import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class SentenceComplexityAnalyser {

	private static final Map<Integer, String> FILE_NAME_BY_YEAR;
	static {
		FILE_NAME_BY_YEAR = new TreeMap<Integer, String>();

		FILE_NAME_BY_YEAR.put(2007, "dataset_2007_sen.arff");
		FILE_NAME_BY_YEAR.put(2010, "dataset_2010_sen.arff");
		FILE_NAME_BY_YEAR.put(2011, "dataset_2011_sen.arff");
		FILE_NAME_BY_YEAR.put(2012, "dataset_2012_sen.arff");

		// FILE_NAME_BY_YEAR.put(2007, "dataset_sen_normalized_2007.arff");
		// FILE_NAME_BY_YEAR.put(2010, "dataset_sen_normalized_2010.arff");
		// FILE_NAME_BY_YEAR.put(2011, "dataset_sen_normalized_2011.arff");
		// FILE_NAME_BY_YEAR.put(2012, "dataset_sen_normalized_2012.arff");
	}

	private Instances instances;

	public static void main(String[] args) {

		// int year = 2011;
		// String fileName = FILE_NAME_BY_YEAR.get(year);
		//
		// SentenceComplexityAnalyser analyser = new SentenceComplexityAnalyser(
		// fileName);
		// analyser.printOutComplexities(true);
		// System.out.println("----------------------------");
		// analyser.printOutComplexities(false);
		// System.out.println("========================================");

		for (Entry<Integer, String> fileNameByYear : FILE_NAME_BY_YEAR
				.entrySet()) {
			SentenceComplexityAnalyser analyser = new SentenceComplexityAnalyser(
					fileNameByYear.getValue());
			System.out.println(fileNameByYear.getKey());
			System.out.println("POS: " + analyser.measureSSE(true));
			System.out.println("NEG: " + analyser.measureSSE(false));
			System.out.println("========================================");
		}
	}

	public SentenceComplexityAnalyser(String fileName) {
		ArffLoader arffLoader = new ArffLoader();
		try {
			arffLoader.setSource(SentenceComplexityAnalyser.class
					.getResourceAsStream(fileName));
			instances = arffLoader.getDataSet();
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}
	}

	public void printOutComplexities(boolean isPositive) {
		List<List<Double>> complexityList = getComplexityList(isPositive);
		Assert.isTrue(complexityList.size() == instances.numAttributes() - 1);

		for (List<Double> complexity : complexityList) {
			// double mean = computeMean(complexity);
			// System.out.println(mean);
			for (Double value : complexity) {
				System.out.print(value + "\t");
			}
			System.out.println();
		}
	}

	public double measureSSE(Boolean isPositive) {

		List<List<Double>> complexityList = getComplexityList(isPositive);
		Assert.isTrue(complexityList.size() == instances.numAttributes() - 1);

		double totalSSE = 0.0;
		int num = -1;
		for (List<Double> eachComplexity : complexityList) {
			Collections.sort(eachComplexity);
			// System.out.println(eachComplexity);

			if (num == -1) {
				num = eachComplexity.size();
			} else {
				Assert.isTrue(eachComplexity.size() == num);
			}

			// double mean = computeMean(eachComplexity);
			// double avgSSE = computeSSE(eachComplexity, mean)
			// / eachComplexity.size();
			double avgSSE = new Variance().evaluate(ArrayUtils
					.toPrimitive(eachComplexity
							.toArray(new Double[eachComplexity.size()])));

			totalSSE += avgSSE;
		}

		return totalSSE /* / complexityList.size() */;
	}

	// private double computeSSE(List<Double> eachComplexity, double mean) {
	// double sse = 0.0;
	// for (Double num : eachComplexity) {
	// sse += (num - mean) * (num - mean);
	// }
	//
	// return sse;
	// }
	//
	// private double computeMean(List<Double> eachComplexity) {
	// double sum = 0.0;
	// for (Double num : eachComplexity) {
	// sum += num;
	// }
	//
	// return sum / eachComplexity.size();
	// }

	private List<List<Double>> getComplexityList(Boolean isPositive) {

		List<List<Double>> complexityList = new ArrayList<List<Double>>(
				instances.numAttributes() - 1);
		for (int index = 0; index < instances.numAttributes() - 1; index++) {
			complexityList.add(new ArrayList<Double>(instances.numInstances()));
		}

		for (int paperIndex = 0; paperIndex < instances.numInstances(); paperIndex++) {
			Instance instance = instances.instance(paperIndex);

			Assert.isTrue(instances.attribute(instances.numAttributes() - 1)
					.isNominal());
			String classValue = instance
					.toString(instances.numAttributes() - 1);
			Assert.isTrue(Boolean.TRUE.toString().equals(classValue)
					|| Boolean.FALSE.toString().equals(classValue));

			if (!isPositive.toString().equals(classValue)) {
				continue;
			}

			for (int complexIndex = 0; complexIndex < instances.numAttributes() - 1; complexIndex++) {
				double count4ComplexInPeper = instance.value(complexIndex);
				complexityList.get(complexIndex).add(count4ComplexInPeper);
			}
		}

		return complexityList;
	}

}
