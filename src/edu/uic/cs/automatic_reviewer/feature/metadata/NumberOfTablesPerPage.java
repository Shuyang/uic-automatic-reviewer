package edu.uic.cs.automatic_reviewer.feature.metadata;

import java.util.Map;

import edu.uic.cs.automatic_reviewer.input.Metadata;

public class NumberOfTablesPerPage extends AbstractMetadata {

	@Override
	public String getName() {
		return "#TABLE";
	}

	@Override
	protected Map<Integer, Integer> getMetadataCountByPage(Metadata metadata) {
		return metadata.getNumOfTablesByPage();
	}

}
