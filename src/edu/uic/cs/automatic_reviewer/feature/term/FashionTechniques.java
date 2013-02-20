package edu.uic.cs.automatic_reviewer.feature.term;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import edu.uic.cs.automatic_reviewer.common.AbstractWordOperations;
import edu.uic.cs.automatic_reviewer.feature.Feature;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.PaperCache;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;

public class FashionTechniques extends AbstractWordOperations implements
		Feature {

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

	private static TreeMap<String, Set<String>> TECHNIQUE_ALTERNATIVE_NAMES = null;
	private static Map<String, Set<Pattern>> TECHNIQUE_ALTERNATIVE_NAME_PATTERNS = null;

	public TreeMap<String, Set<String>> getAllFashionTechniques() {
		if (TECHNIQUE_ALTERNATIVE_NAMES == null) {
			synchronized (FashionTechniques.class) {
				if (TECHNIQUE_ALTERNATIVE_NAMES == null) {
					TECHNIQUE_ALTERNATIVE_NAMES = parseTechniquesAlternativeNames();
				}
			}
		}

		Assert.notEmpty(TECHNIQUE_ALTERNATIVE_NAMES);
		return TECHNIQUE_ALTERNATIVE_NAMES;
	}

	private TreeMap<String, Set<String>> parseTechniquesAlternativeNames() {
		TreeMap<String, Set<String>> techniquesAlternativeNames = new TreeMap<String, Set<String>>();

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

	private Map<String, Set<Pattern>> getAllFashionTechniquePatterns() {
		if (TECHNIQUE_ALTERNATIVE_NAME_PATTERNS == null) {
			synchronized (FashionTechniques.class) {
				if (TECHNIQUE_ALTERNATIVE_NAME_PATTERNS == null) {
					TECHNIQUE_ALTERNATIVE_NAME_PATTERNS = parseTechniquesAlternativeNamePatterns();
				}
			}
		}

		Assert.notEmpty(TECHNIQUE_ALTERNATIVE_NAME_PATTERNS);
		return TECHNIQUE_ALTERNATIVE_NAME_PATTERNS;
	}

	private Map<String, Set<Pattern>> parseTechniquesAlternativeNamePatterns() {
		Map<String, Set<Pattern>> result = new TreeMap<String, Set<Pattern>>();

		for (Entry<String, Set<String>> entry : getAllFashionTechniques()
				.entrySet()) {
			String techName = entry.getKey();
			Set<String> alternativeNames = entry.getValue();
			Set<Pattern> patterns = new HashSet<Pattern>(
					alternativeNames.size());

			for (String alternativeName : alternativeNames) {
				Pattern pattern = createMatchPattern(alternativeName);
				patterns.add(pattern);
			}

			result.put(techName, patterns);
		}

		return result;
	}

	private Pattern createMatchPattern(String term) {
		String patternString = "\\b" + term.replace(".", "\\.") + "(\\b|\\s)";
		return Pattern.compile(patternString, Pattern.CASE_INSENSITIVE);
	}

	// in memory cache
	private static final Map<String, TreeMap<String, Integer>> TECHNIQUE_FREQUENCIES_CACHE = new HashMap<String, TreeMap<String, Integer>>();

	public TreeMap<String, Integer> techniqueFrequenciesInPaper(Paper paper) {

		String paperName = paper.getMetadata().getPaperFileName();
		TreeMap<String, Integer> result = TECHNIQUE_FREQUENCIES_CACHE
				.get(paperName);
		if (result != null) {
			return result;
		}

		result = new TreeMap<String, Integer>();
		// initialize the result frequencies
		for (String techName : getAllFashionTechniquePatterns().keySet()) {
			result.put(techName, Integer.valueOf(0));
		}

		for (Entry<String, Set<Pattern>> entry : getAllFashionTechniquePatterns()
				.entrySet()) {
			String techName = entry.getKey();
			Set<Pattern> alternativeNamePatterns = entry.getValue();

			// /////////////////////////////////////////////////////////////////
			int count = 0;

			String abstractParagraph = paper.getAbstract();
			count += countMentions(abstractParagraph, alternativeNamePatterns);

			for (String paragraph : paper.getContentParagraphs()) {
				count += countMentions(paragraph, alternativeNamePatterns);
			}

			result.put(techName, count);
		}

		TECHNIQUE_FREQUENCIES_CACHE.put(paperName, result);

		return result;
	}

	private int countMentions(String paragraph,
			Set<Pattern> alternativeNamePatterns) {
		if (paragraph == null) {
			return 0;
		}

		int result = 0;
		for (Pattern pattern : alternativeNamePatterns) {
			Matcher matcher = pattern.matcher(paragraph);
			while (matcher.find()) {
				result++;
			}
		}
		return result;
	}

	// public Map<String, Boolean> techniquesMentionedInPaper(Paper paper) {
	// Map<String, Boolean> result = new TreeMap<String, Boolean>();
	//
	// techName: for (Entry<String, Set<String>> entry :
	// getAllFashionTechniques()
	// .entrySet()) {
	//
	// String name = entry.getKey();
	//
	// Set<String> alternativeNames = new HashSet<String>(entry.getValue()
	// .size());
	// for (String alterName : entry.getValue()) {
	// // make sure each term be considered as words rather than part
	// // of a term
	// alternativeNames.add(" " + alterName + " ");
	// }
	//
	// // /////////////////////////////////////////////////////////////////
	// String abstractParagraph = paper.getAbstract();
	// if (findMentionOfTechnique(abstractParagraph, alternativeNames)) {
	// result.put(name, true);
	// continue techName;
	// }
	// for (String paragraph : paper.getContentParagraphs()) {
	// if (findMentionOfTechnique(paragraph, alternativeNames)) {
	// result.put(name, true);
	// continue techName;
	// }
	// }
	//
	// result.put(name, false);
	// }
	//
	// return result;
	// }
	//
	// private boolean findMentionOfTechnique(String paragraph,
	// Set<String> alternativeNames) {
	// return containsAny(paragraph, alternativeNames, true);
	// }

	public TreeMap<String, Boolean> techniquesMentionedInPaper(Paper paper) {
		TreeMap<String, Boolean> result = new TreeMap<String, Boolean>();
		Map<String, Integer> frequency = techniqueFrequenciesInPaper(paper);
		for (Entry<String, Integer> entry : frequency.entrySet()) {
			result.put(entry.getKey(), entry.getValue().intValue() > 0 ? true
					: false);
		}

		return result;
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
		List<Paper> papers = PaperCache.getInstance().getAllPapers();
		Collections.sort(papers, new Comparator<Paper>() {
			@Override
			public int compare(Paper o1, Paper o2) {
				return o1.getMetadata().getPaperFileName()
						.compareTo(o2.getMetadata().getPaperFileName());
			}
		});

		Map<String, Integer> paperCountByTerm = new TreeMap<String, Integer>();
		for (Paper paper : papers) {
			Map<String, Boolean> techniquesMentionedInPaper = fashionTechniques
					.techniquesMentionedInPaper(paper);
			Map<String, Integer> techniqueFrequenciesInPaper = fashionTechniques
					.techniqueFrequenciesInPaper(paper);

			System.out.print(paper.getMetadata().getPaperFileName() + " | ");
			for (Entry<String, Boolean> entry : techniquesMentionedInPaper
					.entrySet()) {
				if (entry.getValue()) {
					Integer count = paperCountByTerm.get(entry.getKey());
					paperCountByTerm.put(entry.getKey(), (count == null) ? 1
							: (count + 1));

					System.out.print(entry.getKey() + "["
							+ techniqueFrequenciesInPaper.get(entry.getKey())
							+ "], ");
				}
			}
			System.out.println();

			// Map<String, Integer> techniqueFrequenciesInPaper =
			// fashionTechniques
			// .techniqueFrequenciesInPaper(paper);
			// System.out.println(techniqueFrequenciesInPaper);
		}

		System.out.println("===============================");
		for (Entry<String, Integer> entry : paperCountByTerm.entrySet()) {
			System.out.println(entry);
		}
	}

	@Override
	public double[] getInstanceValues(Paper paper) {

		TreeMap<String, Integer> techniqueFrequencies = techniqueFrequenciesInPaper(paper);
		double[] result = new double[techniqueFrequencies.size()];

		int index = 0;
		for (Integer frequency : techniqueFrequencies.values()) {
			result[index++] = frequency.doubleValue();
		}

		return result;
	}

	@Override
	public int getNumberOfSubFeatures() {
		return TECHNIQUES.size();
	}

	@Override
	public String[] getSubFeatureNames() {
		Collection<String> names = getAllFashionTechniques().keySet();
		String[] result = new String[names.size()];

		int index = 0;
		for (String name : names) {
			result[index++] = getName() + "_" + name;
		}

		return result;
	}

	@Override
	public String getName() {
		return "TECH_TERM";
	}
}
