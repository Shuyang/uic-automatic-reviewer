package edu.uic.cs.automatic_reviewer.feature;

public interface FeatureDefinition {

	// /**
	// * If the feature is numeric, {@link #getDomain()} must return
	// * <code>null</code>
	// *
	// * @return
	// */
	// boolean isNumeric();
	//
	// /**
	// * If {@link #isNumeric()} return true, this method must return
	// * <code>null</code>
	// *
	// * @return
	// */
	// FastVector getDomain();

	/**
	 * Return feature name
	 * 
	 * @return
	 */
	String getName();

	String[] getSubFeatureNames();

	/**
	 * If this feature is composite, this number will be greater than 1; the
	 * {@link #getName()} will be used as name prefix, indices("_0", "_1"...)
	 * will be appended to the prefix.
	 * 
	 * @return
	 */
	int getNumberOfSubFeatures();
}
