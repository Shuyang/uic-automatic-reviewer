package edu.uic.cs.automatic_reviewer.input;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.uic.cs.automatic_reviewer.misc.Assert;

public class Paper {

	public static class Author {
		private String name;
		private String organization;
		private String email;

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getOrganization() {
			return organization;
		}

		public void setOrganization(String organization) {
			this.organization = organization;
		}

		public String getEmail() {
			return email;
		}

		public void setEmail(String email) {
			this.email = email;
		}

		@Override
		public String toString() {
			return "Author [name=" + name + ", email=" + email
					+ ", organization=" + organization + "]";
		}

	}

	// public static class Page {
	//
	// private List<String> rawParagraphs = new ArrayList<String>();
	//
	// public void addRawParagraph(String rawParagraph) {
	// this.rawParagraphs.add(rawParagraph);
	// }
	//
	// public List<String> getRawParagraphs() {
	// return rawParagraphs;
	// }
	// }

	public static class Paragraph {
		private String content;
		private Integer pageNum;

		public Paragraph(String content, Integer pageNum) {
			this.content = content;
			this.pageNum = pageNum;
		}

		public String getContent() {
			return content;
		}

		public Integer getPageNum() {
			return pageNum;
		}

	}

	// /////////////////////////////////////////////////////////////////////////
	private Metadata metadata;

	private String title;
	private List<Author> authors;

	private String abstractParagraph;

	private int numOfPages;

	private Map<Integer, List<String>> contentParagraphsByPage = new HashMap<Integer, List<String>>();
	private List<String> contentParagraphs = new ArrayList<String>();
	private List<String> references = new ArrayList<String>();

	public void setNumOfPages(int numOfPages) {
		this.numOfPages = numOfPages;
	}

	public int getNumOfPages() {
		return numOfPages;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getAbstract() {
		return abstractParagraph;
	}

	public void setAbstract(String abstractParagraph) {
		this.abstractParagraph = abstractParagraph;
	}

	public void addReference(String reference) {
		this.references.add(reference);
	}

	public List<String> getReferences() {
		return references;
	}

	public void addContentParagraph(Paragraph paragraph) {

		Integer pageNum = paragraph.getPageNum();
		Assert.isTrue(pageNum.intValue() <= numOfPages, "Page number ["
				+ pageNum.intValue() + "] can not exceed number of pages ["
				+ numOfPages + "]");

		List<String> paragraphs = contentParagraphsByPage.get(pageNum);
		if (paragraphs == null) {
			paragraphs = new ArrayList<String>();
			contentParagraphsByPage.put(pageNum, paragraphs);
		}
		paragraphs.add(paragraph.getContent());

		this.contentParagraphs.add(paragraph.getContent());
	}

	public List<String> getContentParagraphs() {
		return contentParagraphs;
	}

	/**
	 * 
	 * @param pageNum
	 *            based on "1"!
	 * @return
	 */
	public List<String> getContentParagraphsOnPage(Integer pageNum) {
		return contentParagraphsByPage.get(pageNum);
	}

	public List<Author> getAuthors() {
		return authors;
	}

	public void setAuthors(List<Author> authors) {
		this.authors = authors;
	}

	public Metadata getMetadata() {
		return metadata;
	}

	public void setMetadata(Metadata metadata) {
		this.metadata = metadata;
	}

}
