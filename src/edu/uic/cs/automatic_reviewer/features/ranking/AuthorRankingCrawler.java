package edu.uic.cs.automatic_reviewer.features.ranking;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.log4j.Logger;
import org.htmlparser.Attribute;
import org.htmlparser.Node;
import org.htmlparser.NodeFilter;
import org.htmlparser.Parser;
import org.htmlparser.Tag;
import org.htmlparser.filters.AndFilter;
import org.htmlparser.filters.NodeClassFilter;
import org.htmlparser.tags.Div;
import org.htmlparser.tags.LinkTag;
import org.htmlparser.util.NodeList;

import edu.uic.cs.automatic_reviewer.input.Paper.Author;
import edu.uic.cs.automatic_reviewer.misc.AutomaticReviewerException;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;

class AuthorRankingCrawler {

	private static final String AUTHOR_NODE_ID_SUFFIX = "AuthorSearchItem_divContent";
	private static final String AUTHOR_NAME_NODE_ID_SUFFIX = "AuthorSearchItem_authorName";
	private static final String AFFILIATION_NODE_ID_SUFFIX = "AuthorSearchItem_affiliation";

	private static final String URL_RANKING_START_PLACE_HOLDER = "${start}";
	private static final String URL_RANKING_END_PLACE_HOLDER = "${end}";
	private static final String URL = "http://academic.research.microsoft.com/RankList?entitytype=2&topDomainID=2&subDomainID=9&last=0&start="
			+ URL_RANKING_START_PLACE_HOLDER
			+ "&end="
			+ URL_RANKING_END_PLACE_HOLDER + "";

	public static final int NUMBER_OF_AUTHORS_TO_RETRIEVE = 2000;

	private static final Logger LOGGER = LogHelper
			.getLogger(AuthorRankingCrawler.class);

	private static class AuthorNodeFilter implements NodeFilter {

		private static final long serialVersionUID = 1L;

		private String idValueSuffix;

		public AuthorNodeFilter(String idValueSuffix) {
			this.idValueSuffix = idValueSuffix;
		}

		@Override
		public boolean accept(Node node) {
			if (node instanceof Tag) {
				Tag tag = (Tag) node;
				Attribute attribute = tag.getAttributeEx("id");
				if (attribute == null) {
					return false;
				}

				return attribute.getValue() != null
						&& attribute.getValue().endsWith(idValueSuffix);
			}

			return false;
		}
	}

	private Parser parser = new Parser();

	public List<Author> crawlAuthors(int number) {

		List<Author> result = new ArrayList<Author>(number);

		for (int index = 0; index < number / 100; index++) {
			int start = index * 100 + 1;
			int end = index * 100 + 100;

			String url = URL
					.replace(URL_RANKING_START_PLACE_HOLDER, start + "")
					.replace(URL_RANKING_END_PLACE_HOLDER, end + "");

			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug(url);
			}

			List<Author> authors = crawlAuthors(url);
			result.addAll(authors);
		}

		return result;
	}

	private List<Author> crawlAuthors(String url) {

		NodeList nodeList = null;
		try {
			parser.setResource(url);
			nodeList = parser.parse(new AndFilter(new NodeFilter[] {
					new NodeClassFilter(Div.class),
					new AuthorNodeFilter(AUTHOR_NODE_ID_SUFFIX) }));

		} catch (Exception e) {
			throw new AutomaticReviewerException(e);
		}

		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(nodeList.size()
					+ " authors' infomation has been retrieved from [" + url
					+ "]");
		}
		List<Author> result = new ArrayList<Author>(nodeList.size());
		for (int index = 0; index < nodeList.size(); index++) {
			Node authorNode = nodeList.elementAt(index);
			Author author = parseAuthorNode(authorNode);
			result.add(author);

			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug(author);
			}
		}

		return result;
	}

	private Author parseAuthorNode(Node authorNode) {

		NodeList authorNameNL = new NodeList();
		authorNode.collectInto(authorNameNL, new AndFilter(new NodeFilter[] {
				new NodeClassFilter(LinkTag.class),
				new AuthorNodeFilter(AUTHOR_NAME_NODE_ID_SUFFIX) }));

		String authorName = authorNameNL.elementAt(0).toPlainTextString();
		int parenthesesIndex = authorName.indexOf('(');
		if (parenthesesIndex != -1) {
			authorName = StringEscapeUtils.unescapeHtml4(authorName.substring(
					0, parenthesesIndex).trim());
		}

		NodeList affiliationNL = new NodeList();
		authorNode.collectInto(affiliationNL, new AndFilter(new NodeFilter[] {
				new NodeClassFilter(LinkTag.class),
				new AuthorNodeFilter(AFFILIATION_NODE_ID_SUFFIX) }));
		String organization = "";
		if (affiliationNL != null && affiliationNL.size() != 0) {
			organization = StringEscapeUtils.unescapeHtml4(affiliationNL
					.elementAt(0).toPlainTextString());
		}

		Author result = new Author();
		result.setName(authorName);
		result.setOrganization(organization);

		return result;
	}

	public static void main(String[] args) {
		List<Author> result = new AuthorRankingCrawler().crawlAuthors(300);
		for (Author author : result) {
			System.out.println(author);
		}
	}

}
