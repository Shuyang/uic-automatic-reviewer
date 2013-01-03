package edu.uic.cs.automatic_reviewer.feature;

import edu.uic.cs.automatic_reviewer.input.Paper;

public interface InstanceCreator {

	double[] getInstanceValues(Paper paper);
}
