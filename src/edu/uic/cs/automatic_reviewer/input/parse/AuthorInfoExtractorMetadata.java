package edu.uic.cs.automatic_reviewer.input.parse;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.tika.metadata.Metadata;

import edu.uic.cs.automatic_reviewer.common.Constants;
import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;

@SuppressWarnings("deprecation")
public class AuthorInfoExtractorMetadata extends AuthorInfoExtractorRegex
		implements Constants.InputParse {

	private static final Pattern COMPOUND_EMAIL_PATTERN = Pattern
			.compile("\\{([_a-z0-9-]+(\\.[_a-z0-9-]+)*\\s*,\\s*)*[_a-z0-9-]+(\\.[_a-z0-9-]+)*}@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4})");
	private static final Pattern EMAIL_PATTERN = Pattern
			.compile("[_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4})");

	public void parseAndFillAuthorInfos(Paper paper,
			List<String> rawAuthorInfos, Metadata tikaMetadata) {

		List<String> authorNames = gatherAuthorNamesFromTikaMetadata(tikaMetadata);
		if (authorNames.isEmpty()) {
			super.parseAndFillAuthorInfos(paper, rawAuthorInfos, tikaMetadata);
			return;
		}

		List<Author> authors = gatherEmailAndOrganizationInfos(authorNames,
				rawAuthorInfos);

		paper.setAuthors(authors);

	}

	private List<String> gatherAuthorNamesFromTikaMetadata(Metadata tikaMetadata) {

		String authorString = tikaMetadata.get("Author");
		if (authorString == null || authorString.trim().length() == 0) {
			return Collections.emptyList();
		}

		if (authorString.toLowerCase()
				.contains(ILLEGAL_AUTHOR_NAME_IN_LOW_CASE)) {
			return Collections.emptyList();
		}

		List<String> result = new ArrayList<String>();
		String[] authors = authorString.split(";");
		for (String author : authors) {
			result.add(author.trim());
		}

		return result;
	}

	protected List<Author> gatherEmailAndOrganizationInfos(
			List<String> authorNames, List<String> rawAuthorInfos) {

		Map<String, String> infoByName = gatherInfosByAuthorName(authorNames,
				rawAuthorInfos);

		List<Author> result = new ArrayList<Author>(infoByName.size());
		for (Entry<String, String> entry : infoByName.entrySet()) {

			String name = entry.getKey();
			String info = entry.getValue();

			Author author = new Author();
			author.setName(name);

			String organization = info;
			String email = "";

			Matcher matcher = EMAIL_PATTERN.matcher(info);
			if (matcher.find()) {
				email = matcher.group();
				organization = info.replace(email, "");
			} else {
				matcher = COMPOUND_EMAIL_PATTERN.matcher(info);
				if (matcher.find()) {
					email = matcher.group();
					organization = info.replace(email, "");
				}
			}

			author.setOrganization(organization.trim());
			author.setEmail(email);

			result.add(author);
		}

		return result;
	}

	private Map<String, String> gatherInfosByAuthorName(
			List<String> authorNames, List<String> rawAuthorInfos) {

		rawAuthorInfos = splitByLine(rawAuthorInfos);

		List<String> replacedRawInfos = new ArrayList<String>();
		Map<String, Integer> usedMaxIndexForTerm = new HashMap<String, Integer>();

		for (String rawInfo : rawAuthorInfos) {
			rawInfo = rawInfo.replace("*", "");
			String[] terms = rawInfo.split("\\s+");

			for (String term : terms) {
				int nameIndex = findNameIndex(term, authorNames,
						usedMaxIndexForTerm);
				if (nameIndex == -1) {
					continue;
				}

				rawInfo = rawInfo.replaceFirst(term, "{" + nameIndex + "}");
			}

			replacedRawInfos.add(rawInfo);
		}

		boolean[] lineContainsNameOrNot = new boolean[replacedRawInfos.size()];
		for (int lineIndex = 0; lineIndex < replacedRawInfos.size(); lineIndex++) {
			String replacedRawInfo = replacedRawInfos.get(lineIndex);
			if (replacedRawInfo.matches(".*\\{\\d}.*")) {
				lineContainsNameOrNot[lineIndex] = true;
			} else {
				lineContainsNameOrNot[lineIndex] = false;
			}
		}

		Map<String, Integer> appearingLineIndexByName = new HashMap<String, Integer>();
		for (int currentNameIndex = 0; currentNameIndex < authorNames.size(); currentNameIndex++) {
			String nameSymbol = "{" + currentNameIndex + "}";

			for (int lineIndex = 0; lineIndex < replacedRawInfos.size(); lineIndex++) {
				String replacedRawInfo = replacedRawInfos.get(lineIndex);
				if (replacedRawInfo.contains(nameSymbol)) {
					appearingLineIndexByName.put(
							authorNames.get(currentNameIndex), lineIndex);
				}
			}
		}

		// use linkedHashMap to maintain the order of authors
		Map<String, String> infoByName = new LinkedHashMap<String, String>();
		for (String name : authorNames) {
			Integer appearingIndex = appearingLineIndexByName.get(name);
			if (appearingIndex == null) {
				// use the entire raw info
				infoByName.put(name, concatenate(rawAuthorInfos));
				continue;
			}

			StringBuilder info = new StringBuilder();
			boolean infoStart = false;
			for (int index = appearingIndex.intValue() + 1; index < lineContainsNameOrNot.length; index++) {

				if (infoStart) {
					if (lineContainsNameOrNot[index]) {
						break; // another name start
					} else {
						info.append(replacedRawInfos.get(index)).append("\n");
					}
				} else { // we haven't met any info
					if (lineContainsNameOrNot[index]) {
						continue;
					} else { // not name, means it is the beginning of info
						info.append(replacedRawInfos.get(index)).append("\n");
						infoStart = true;
					}
				}
			}

			String authorInfo = info.toString().trim();
			infoByName.put(name, authorInfo);
		}
		return infoByName;
	}

	private String concatenate(List<String> rawAuthorInfos) {

		StringBuilder info = new StringBuilder();
		for (String str : rawAuthorInfos) {
			info.append(str).append("\n");
		}
		return info.toString().trim();
	}

	private List<String> splitByLine(List<String> rawAuthorInfos) {
		List<String> result = new ArrayList<String>();
		for (String rawLine : rawAuthorInfos) {
			String[] lines = rawLine.split("\n");
			result.addAll(Arrays.asList(lines));
		}
		return result;
	}

	private String removeDiacritics(String input) {

		String nrml = Normalizer.normalize(input, Normalizer.Form.NFD);
		StringBuilder stripped = new StringBuilder();
		for (int i = 0; i < nrml.length(); ++i) {
			if (Character.getType(nrml.charAt(i)) != Character.NON_SPACING_MARK) {
				stripped.append(nrml.charAt(i));
			}
		}
		return stripped.toString();
	}

	private int findNameIndex(String term, List<String> authorNames,
			Map<String, Integer> usedMaxIndexForTerm) {

		if (StringUtils.containsAny(term, '(', ')')) {
			return -1;
		}

		term = term.replaceAll("\\W", ""); // TODO may have problem here
		if (term.length() == 0) { // none character
			return -1;
		}

		for (int index = 0; index < authorNames.size(); index++) {
			String name = removeDiacritics(authorNames.get(index));
			if (name.contains(term)) {

				Integer usedMaxIndex = usedMaxIndexForTerm.get(term);
				if (usedMaxIndex == null || usedMaxIndex.intValue() < index) {
					usedMaxIndexForTerm.put(term, index);
				} else {
					continue;
				}
			}
		}

		Integer currentUsedMaxIndex = usedMaxIndexForTerm.get(term);
		return currentUsedMaxIndex == null ? -1 : currentUsedMaxIndex
				.intValue();
	}

}
