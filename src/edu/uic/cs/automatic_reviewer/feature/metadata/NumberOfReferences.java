package edu.uic.cs.automatic_reviewer.feature.metadata;

import java.util.List;

import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;

public class NumberOfReferences implements Feature {

	@Override
	public String getName() {
		return "#REF";
	}

	@Override
	public String[] getSubFeatureNames() {
		return new String[] { getName() };
	}

	@Override
	public int getNumberOfSubFeatures() {
		return 1;
	}

	@Override
	public double[] getInstanceValues(Paper paper) {
		List<String> references = paper.getReferences();
		if (references == null || references.size() < 5) {
			return new double[] { Double.NaN };
		}
		return new double[] { references.size() };
	}
}
