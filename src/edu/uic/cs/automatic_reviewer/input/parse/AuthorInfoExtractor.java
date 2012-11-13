package edu.uic.cs.automatic_reviewer.input.parse;

import java.util.List;

import org.apache.tika.metadata.Metadata;

import edu.uic.cs.automatic_reviewer.input.Paper;

public interface AuthorInfoExtractor {
	void parseAndFillAuthorInfos(Paper paper, List<String> rawAuthorInfos,
			Metadata tikaMetadata);
}
