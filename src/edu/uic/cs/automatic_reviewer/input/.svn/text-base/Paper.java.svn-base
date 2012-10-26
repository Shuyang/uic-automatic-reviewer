package edu.uic.cs.automatic_reviewer.input;

import java.util.ArrayList;
import java.util.List;

public class Paper {

	// public static class Paragraph {
	//
	// private String content;
	//
	// public Paragraph(String content) {
	// this.content = content;
	// }
	//
	// public String getContent() {
	// return content;
	// }
	// }

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

	public static class Page {

		private List<String> rawParagraphs = new ArrayList<String>();

		public void addRawParagraph(String rawParagraph) {
			this.rawParagraphs.add(rawParagraph);
		}

		public List<String> getRawParagraphs() {
			return rawParagraphs;
		}
	}

	// /////////////////////////////////////////////////////////////////////////
	private Metadata metadata;

	private String title;
	private List<Author> authors;

	private String abstractParagraph;
	// private List<String> paragraphs;
	private List<String> references = new ArrayList<String>();

	private List<Page> pages = new ArrayList<Page>();

	// private List<Paragraph> paragraphs;

	public void addPage(Page page) {
		this.pages.add(page);
	}

	public int getNumOfPages() {
		return pages.size();
	}

	// public List<Paragraph> getParagraphs() {
	// return paragraphs;
	// }
	//
	// public void setParagraphs(List<Paragraph> paragraphs) {
	// this.paragraphs = paragraphs;
	// }

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

	public List<Author> getAuthors() {
		return authors;
	}

	public void setAuthors(List<Author> authors) {
		this.authors = authors;
	}
}
