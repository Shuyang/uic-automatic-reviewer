package edu.uic.cs.automatic_reviewer.feature.term;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import opennlp.tools.chunker.ChunkerME;
import opennlp.tools.chunker.ChunkerModel;
import opennlp.tools.util.Span;
import edu.stanford.nlp.ling.TaggedWord;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class OpenNLPChunker {
	public static enum ChunkType {
		NP, VP, PP, ADJP, ADVP, O;
	}

	private static final String CHUNKER_MODEL_NAME = "en-chunker.bin";
	private ChunkerModel model = null;

	private synchronized ChunkerModel getChunkerModel() {
		if (model != null) {
			return model;
		}

		try {
			InputStream modelIn = OpenNLPChunker.class
					.getResourceAsStream(CHUNKER_MODEL_NAME);
			model = new ChunkerModel(modelIn);
		} catch (IOException ioe) {
			throw new AutomaticReviewerException(ioe);
		}

		return model;
	}

	public List<Span> getChunkSpans(List<TaggedWord> taggedTerms,
			ChunkType chunkType) {
		String[] terms = new String[taggedTerms.size()];
		String[] tags = new String[taggedTerms.size()];
		int index = 0;
		for (TaggedWord taggedTerm : taggedTerms) {
			terms[index] = taggedTerm.word();
			tags[index] = taggedTerm.tag();
			index++;
		}

		ChunkerME chunker = new ChunkerME(getChunkerModel());
		Span[] spans = chunker.chunkAsSpans(terms, tags);

		List<Span> result = new ArrayList<Span>();
		for (Span span : spans) {
			if (chunkType.name().equals(span.getType())) {
				result.add(span);
			}
		}

		return result;
	}

	public List<List<TaggedWord>> getChunks(List<TaggedWord> taggedTerms,
			ChunkType chunkType) {
		List<Span> spans = getChunkSpans(taggedTerms, chunkType);

		List<List<TaggedWord>> result = new ArrayList<List<TaggedWord>>();

		for (Span span : spans) {
			List<TaggedWord> phrase = taggedTerms.subList(span.getStart(),
					span.getEnd());
			result.add(phrase);
		}

		return result;
	}
}
