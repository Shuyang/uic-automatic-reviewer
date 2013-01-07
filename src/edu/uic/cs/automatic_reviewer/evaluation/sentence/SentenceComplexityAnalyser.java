package edu.uic.cs.automatic_reviewer.evaluation.sentence;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class SentenceComplexityAnalyser {

	private static final String FILE_NAME = "dataset_replaced_missing_normalized_best_SEN_COMPX_2012.arff";

	private Instances instances;

	public static void main(String[] args) {
		SentenceComplexityAnalyser analyser = new SentenceComplexityAnalyser();
		System.out.println(analyser.measureSSE(true));
		System.out.println(analyser.measureSSE(false));
	}

	public SentenceComplexityAnalyser() {
		ArffLoader arffLoader = new ArffLoader();
		try {
			arffLoader.setSource(SentenceComplexityAnalyser.class
					.getResourceAsStream(FILE_NAME));
			instances = arffLoader.getDataSet();
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}
	}

	public double measureSSE(Boolean isPositive) {

		List<List<Double>> complexityList = getComplexityList(isPositive);
		Assert.isTrue(complexityList.size() == instances.numAttributes() - 1);

		double totalSSE = 0.0;
		int num = -1;
		for (List<Double> eachComplexity : complexityList) {
			Collections.sort(eachComplexity);
			System.out.println(eachComplexity);

			if (num == -1) {
				num = eachComplexity.size();
			} else {
				Assert.isTrue(eachComplexity.size() == num);
			}

			double mean = computeMean(eachComplexity);
			double avgSSE = computeSSE(eachComplexity, mean)
					/ eachComplexity.size();

			totalSSE += avgSSE;
		}

		return totalSSE / complexityList.size();
	}

	private double computeSSE(List<Double> eachComplexity, double mean) {
		double sse = 0.0;
		for (Double num : eachComplexity) {
			sse += (num - mean) * (num - mean);
		}

		return sse;
	}

	private double computeMean(List<Double> eachComplexity) {
		double sum = 0.0;
		for (Double num : eachComplexity) {
			sum += num;
		}

		return sum / eachComplexity.size();
	}

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
