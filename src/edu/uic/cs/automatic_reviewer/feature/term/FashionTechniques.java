package edu.uic.cs.automatic_reviewer.feature.term;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class FashionTechniques extends AbstractWordOperations {

	private static final Properties TECHNIQUES = new Properties();
	static {
		InputStream inStream = FashionTechniques.class
				.getResourceAsStream("fashionTechniques.properties");

		try {
			TECHNIQUES.load(inStream);
		} catch (IOException e) {
			throw new AutomaticReviewerException(e);
		}
	}

	private Map<String, Set<String>> techniquesAlternativeNames = null;

	public Map<String, Set<String>> getAllFashionTechniques() {
		if (techniquesAlternativeNames == null) {
			synchronized (this) {
				if (techniquesAlternativeNames == null) {
					techniquesAlternativeNames = parseTechniquesAlternativeNames();
				}
			}
		}

		return techniquesAlternativeNames;
	}

	private Map<String, Set<String>> parseTechniquesAlternativeNames() {
		Map<String, Set<String>> techniquesAlternativeNames = new HashMap<String, Set<String>>();

		for (String name : TECHNIQUES.stringPropertyNames()) {
			String alternativeNamesString = TECHNIQUES.getProperty(name);
			String[] alternativeNames = StringUtils.split(
					alternativeNamesString, ",;");

			Set<String> alternativeNameSet = new HashSet<String>();
			for (String alternativeName : alternativeNames) {
				alternativeNameSet.add(alternativeName.trim().toLowerCase(
						Locale.US));
			}

			// alternativeNameSet.add(name.toLowerCase(Locale.US));

			techniquesAlternativeNames.put(name, alternativeNameSet);
		}

		return techniquesAlternativeNames;
	}

	public Map<String, Boolean> techniquesMentionedInPaper(Paper paper) {
		Map<String, Boolean> result = new HashMap<String, Boolean>();

		techName: for (Entry<String, Set<String>> entry : getAllFashionTechniques()
				.entrySet()) {

			String name = entry.getKey();

			Set<String> alternativeNames = new HashSet<String>(entry.getValue()
					.size());
			for (String alterName : entry.getValue()) {
				// make sure each term be considered as words rather than part
				// of a term
				alternativeNames.add(" " + alterName + " ");
			}

			// /////////////////////////////////////////////////////////////////
			String abstractParagraph = paper.getAbstract();
			if (findMentionOfTechnique(abstractParagraph, alternativeNames)) {
				result.put(name, true);
				continue techName;
			}
			for (String paragraph : paper.getContentParagraphs()) {
				if (findMentionOfTechnique(paragraph, alternativeNames)) {
					result.put(name, true);
					continue techName;
				}
			}

			result.put(name, false);
		}

		return result;
	}

	private boolean findMentionOfTechnique(String paragraph,
			Set<String> alternativeNames) {
		return containsAny(paragraph, alternativeNames, true);
	}

	public static void main(String[] args) {
		// Map<String, Set<String>> techniquesAlternativeNames = new
		// FashionTechniques()
		// .getAllFashionTechniques();
		// for (Entry<String, Set<String>> entry : techniquesAlternativeNames
		// .entrySet()) {
		// System.out.println(entry);
		// }

		FashionTechniques fashionTechniques = new FashionTechniques();
		List<Paper> papers = PaperCache.getInstance().getPapers(2012);
		for (Paper paper : papers) {
			Map<String, Boolean> techniquesMentionedInPaper = fashionTechniques
					.techniquesMentionedInPaper(paper);
			System.out.print(paper.getMetadata().getPaperFileName() + " | ");
			for (Entry<String, Boolean> entry : techniquesMentionedInPaper
					.entrySet()) {
				if (entry.getValue()) {
					System.out.print(entry.getKey() + ", ");
				}
			}

			System.out.println();
		}

	}
}
