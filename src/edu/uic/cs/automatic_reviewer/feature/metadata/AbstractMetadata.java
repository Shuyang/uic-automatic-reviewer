package edu.uic.cs.automatic_reviewer.feature.metadata;

import java.util.Map;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Metadata;
import edu.uic.cs.automatic_reviewer.input.Paper;

abstract class AbstractMetadata implements Feature, Constants.Feature {

	@Override
	public int getNumberOfSubFeatures() {
		return MAX_NUMBER_OF_PAGES_PER_PAPER;
	}

	@Override
	public double[] getInstanceValues(Paper paper) {
		Map<Integer, Integer> countByPage = getMetadataCountByPage(paper
				.getMetadata());

		double[] result = new double[MAX_NUMBER_OF_PAGES_PER_PAPER];

		// page start with "1"!!
		for (int page = 1; page <= MAX_NUMBER_OF_PAGES_PER_PAPER; page++) {
			Integer count = countByPage.get(page);
			if (count == null) {
				count = 0;
			}

			result[page - 1] = count.doubleValue();
		}

		return result;
	}

	abstract protected Map<Integer, Integer> getMetadataCountByPage(
			Metadata metadata);
}
