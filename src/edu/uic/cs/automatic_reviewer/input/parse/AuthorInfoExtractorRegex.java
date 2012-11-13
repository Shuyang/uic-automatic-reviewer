package edu.uic.cs.automatic_reviewer.input.parse;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.apache.tika.metadata.Metadata;

import edu.uic.cs.automatic_reviewer.input.Paper;
import edu.uic.cs.automatic_reviewer.input.Paper.Author;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

@Deprecated
public class AuthorInfoExtractorRegex implements AuthorInfoExtractor {

	private static final Logger LOGGER = LogHelper
			.getLogger(AuthorInfoExtractorRegex.class);

	private static final Pattern COMPOUND_EMAIL_PATTERN = Pattern
			.compile("^\\{([_a-z0-9-]+(\\.[_a-z0-9-]+)*\\s*,\\s*)*[_a-z0-9-]+(\\.[_a-z0-9-]+)*}@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4})$");
	private static final Pattern EMAIL_PATTERN = Pattern
			.compile("^[_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4})$");

	@Override
	public void parseAndFillAuthorInfos(Paper paper,
			List<String> rawAuthorInfos, Metadata tikaMetadata) {

		List<Author> authors = new ArrayList<Author>();

		Author author = null;
		StringBuilder organization = new StringBuilder();

		for (String authorInfo : rawAuthorInfos) {

			String[] infos = authorInfo.split("\n");
			for (String info : infos) {

				info = info.trim();
				if (info.length() == 0) {
					continue;
				}

				if (isCompoundEmails(info) && author != null) { // last line
																// usually is
																// the email

					List<String> emails = parseCompoundEmailAddress(info);
					// re-parse names regarding to the emails
					String namesString = author.getName();
					List<Author> reparsedAuthors = reparseAuthors(namesString,
							emails);
					for (Author reparsedAuthor : reparsedAuthors) {
						reparsedAuthor.setOrganization(organization.toString()
								.trim());
						authors.add(reparsedAuthor);

						LOGGER.debug("Parsed " + reparsedAuthor);
					}

					author = null;
					organization.setLength(0);

				} else if (EMAIL_PATTERN.matcher(info).matches()
						&& author != null) {
					// last line usually is the email
					author.setEmail(info);
					author.setOrganization(organization.toString().trim());
					authors.add(author);
					LOGGER.debug("Parsed " + author);

					author = null;
					organization.setLength(0);

				} else if (author == null) { // first line always the name

					author = new Author();
					// replace non-alpha in name
					info = info.replaceAll("[^\\w\\s)]", "");
					author.setName(info);

				} else {
					// we consider the rest part as organization
					organization.append(info).append("\n");
				}
			}
		}

		paper.setAuthors(authors);
	}

	private List<Author> reparseAuthors(String namesString, List<String> emails) {
		// replace non-alpha in name
		namesString = namesString.replaceAll("[^\\w\\s)]", "");
		String[] nameParts = namesString.split("\\s+");
		int emailsNum = emails.size();

		List<Author> result = new ArrayList<Author>();

		if (nameParts.length == emailsNum * 2) { // first+last

			for (int index = 0; index < emailsNum; index++) {

				String name = nameParts[index * 2] + " "
						+ nameParts[index * 2 + 1];

				Author author = new Author();
				author.setName(name);
				author.setEmail(emails.get(index));

				result.add(author);
			}
		} else { // someone's name is first+middle+last
			// TODO find some way to fill this kind of name
			LOGGER.warn("Can not parse author name in string [" + namesString
					+ "]");
		}

		return result;
	}

	private List<String> parseCompoundEmailAddress(String info) {

		int beginIndex = 1;
		int endIndex = info.indexOf("}");

		String prefixString = info.substring(beginIndex, endIndex);
		String[] prefixes = prefixString.split(",\\s*");

		String suffix = info.substring(endIndex + 1);

		List<String> result = new ArrayList<String>(prefixes.length);
		for (String prefix : prefixes) {
			result.add(prefix + suffix);
		}

		return result;
	}

	private boolean isCompoundEmails(String info) {

		// email address like: {hwang207,cyu,sistla}@uic.edu
		return COMPOUND_EMAIL_PATTERN.matcher(info).matches();
	}

	// public static void main(String[] args) {
	//
	// String info = "{hwang207, cyu, sistla}@uic.edu";
	// // System.out.println(COMPOUND_EMAIL_PATTERN.matcher(info).matches());
	// int beginIndex = 1;
	// int endIndex = info.indexOf("}");
	//
	// String prefixString = info.substring(beginIndex, endIndex);
	// String[] prefixes = prefixString.split(",\\s*");
	//
	// String suffix = info.substring(endIndex + 1);
	//
	// List<String> result = new ArrayList<String>(prefixes.length);
	// for (String prefix : prefixes) {
	// result.add(prefix + suffix);
	// }
	//
	// System.out.println(result);
	//
	// }

}
