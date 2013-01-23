package edu.uic.cs.automatic_reviewer.input;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;

import edu.uic.cs.automatic_reviewer.input.parse.PaperParser;
import edu.uic.cs.automatic_reviewer.misc.Assert;
import edu.uic.cs.automatic_reviewer.misc.LogHelper;
import edu.uic.cs.automatic_reviewer.misc.SerializationHelper;

public class PaperCache {

	private static final Logger LOGGER = LogHelper.getLogger(PaperCache.class);

	private static final PaperCache INSTANCE = new PaperCache();

	private static final String PAPER_CACHE_FILE = "data/papers/papers.cache";

	private static final String PAPER_FOLDER = "data/papers/";

	private PaperParser paperParser = new PaperParser();

	private PaperCache() {
	}

	public static PaperCache getInstance() {
		return INSTANCE;
	}

	/**
	 * Get all cached papers for specified year and PaperPublishType(s). <br>
	 * If no PaperPublishType input, all types of papers of the specified year
	 * will be returned. <br>
	 * 
	 * @param year
	 * @param paperPublishTypes
	 * @return
	 */
	public List<Paper> getPapers(int year,
			PaperPublishType... paperPublishTypes) {
		if (paperPublishTypes == null || paperPublishTypes.length == 0) {
			paperPublishTypes = PaperPublishType.values();
		}

		Set<PaperPublishType> types = new TreeSet<PaperPublishType>();
		for (PaperPublishType type : paperPublishTypes) {
			types.add(type);
		}

		Map<PaperPublishType, List<Paper>> papersByPublishType = getPapersByPublishTypeForYear(year);
		Assert.notNull(papersByPublishType);

		List<Paper> result = new ArrayList<Paper>();
		for (PaperPublishType type : types) {
			List<Paper> papers = papersByPublishType.get(type);
			if (papers == null) {
				continue;
			}

			result.addAll(papers);
		}

		return result;
	}

	public Map<PaperPublishType, List<Paper>> getPapersByPublishTypeForYear(
			int year) {
		Map<Integer, Map<PaperPublishType, List<Paper>>> cachedPapersByYear = getCachedPapersByYear();

		Map<PaperPublishType, List<Paper>> result = cachedPapersByYear
				.get(Integer.valueOf(year));
		if (result == null) {
			return Collections.emptyMap();
		}

		return result;
	}

	// public List<Paper> getAllPapers() {
	// Map<Integer, Map<PaperPublishType, List<Paper>>> cachedPapersByYear =
	// getCachedPapersByYear();
	//
	// List<Paper> result = new ArrayList<Paper>();
	// for (Map<PaperPublishType, List<Paper>> papersOfYearByType :
	// cachedPapersByYear
	// .values()) {
	// for (List<Paper> papersOfYear : papersOfYearByType.values()) {
	// result.addAll(papersOfYear);
	// }
	// }
	//
	// return result;
	// }

	public List<Paper> getAllPapers(PaperPublishType... paperPublishTypes) {
		if (paperPublishTypes == null || paperPublishTypes.length == 0) {
			paperPublishTypes = PaperPublishType.values();
		}

		Set<PaperPublishType> types = new TreeSet<PaperPublishType>();
		for (PaperPublishType type : paperPublishTypes) {
			types.add(type);
		}

		Map<Integer, Map<PaperPublishType, List<Paper>>> cachedPapersByYear = getCachedPapersByYear();

		List<Paper> result = new ArrayList<Paper>();
		for (Map<PaperPublishType, List<Paper>> papersOfYearByType : cachedPapersByYear
				.values()) {
			for (Entry<PaperPublishType, List<Paper>> entry : papersOfYearByType
					.entrySet()) {
				PaperPublishType type = entry.getKey();
				if (!types.contains(type)) {
					continue;
				}

				List<Paper> papersOfYear = entry.getValue();
				result.addAll(papersOfYear);
			}
		}

		return result;
	}

	private Map<Integer, Map<PaperPublishType, List<Paper>>> readAndParseAllPapers() {

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN + "Caching all papers...");

		File folder = new File(PAPER_FOLDER);

		Map<Integer, Map<PaperPublishType, List<Paper>>> result = new TreeMap<Integer, Map<PaperPublishType, List<Paper>>>();
		readAndParseAllPapers(folder, result, null);

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END + "Caching all papers... Done.");
		return result;
	}

	@SuppressWarnings("unused")
	private void addNewPapersToCache(int year) {

		Map<Integer, Map<PaperPublishType, List<Paper>>> cachedPapersByYear = getCachedPapersByYear();
		Map<PaperPublishType, List<Paper>> papersOfYear = cachedPapersByYear
				.get(year);
		Assert.isNull(papersOfYear);

		File newYearFolder = new File(PAPER_FOLDER + File.separator + year);
		Assert.isTrue(newYearFolder.isDirectory() && newYearFolder.exists());

		papersOfYear = new HashMap<PaperPublishType, List<Paper>>();
		LOGGER.warn(LogHelper.LOG_LAYER_ONE_BEGIN
				+ "Adding new papers in year [" + papersOfYear + "]...");

		cachedPapersByYear.put(year, papersOfYear);

		for (File child : newYearFolder.listFiles()) {
			readAndParseAllPapers(child, cachedPapersByYear, papersOfYear);
		}

		LOGGER.warn(LogHelper.LOG_LAYER_ONE_END + "Adding new papers in year ["
				+ papersOfYear + "]... Done.");

		// remove old one
		removeCache();
		// create new one
		SerializationHelper.serialize(cachedPapersByYear, PAPER_CACHE_FILE);
	}

	private void readAndParseAllPapers(File file,
			Map<Integer, Map<PaperPublishType, List<Paper>>> result,
			Map<PaperPublishType, List<Paper>> papersOfYear) {

		String fileFullName = file.toString();
		int pathLastPart = fileFullName.lastIndexOf(File.separatorChar);
		String fileName = fileFullName.substring(pathLastPart + 1);

		if (file.isDirectory()) {

			Integer year = null;
			try {
				year = Integer.parseInt(fileName);
			} catch (NumberFormatException ex) {
				// ignore
			}

			if (year != null) {
				papersOfYear = new HashMap<PaperPublishType, List<Paper>>();
				LOGGER.debug("**** " + year + "==============================");
				result.put(year, papersOfYear);
			}

			for (File child : file.listFiles()) {
				readAndParseAllPapers(child, result, papersOfYear);
			}

		} else {
			if (!fileName.endsWith(".pdf")) {
				return;
			}

			if (fileName.startsWith("P")) {
				parseAndStorePaper(file, papersOfYear,
						PaperPublishType.LongPaper);
			} else if (fileName.contains("Student")) {
				parseAndStorePaper(file, papersOfYear,
						PaperPublishType.StudentWorkshopPaper);
			} else if (fileName.startsWith("W")) {
				parseAndStorePaper(file, papersOfYear,
						PaperPublishType.WorkshopPaper);
			} else {
				Assert.isTrue(false);
			}
		}
	}

	private void parseAndStorePaper(File file,
			Map<PaperPublishType, List<Paper>> papersOfYear,
			PaperPublishType publishType) {
		List<Paper> papers = papersOfYear.get(publishType);
		if (papers == null) {
			papers = new ArrayList<Paper>();
			papersOfYear.put(publishType, papers);
		}

		LOGGER.debug("Parsing paper [" + publishType + "|" + file + "]");
		Paper paper = paperParser.parse(file);
		papers.add(paper);
	}

	@SuppressWarnings("unchecked")
	private Map<Integer, Map<PaperPublishType, List<Paper>>> getCachedPapersByYear() {

		Map<Integer, Map<PaperPublishType, List<Paper>>> cachedPapersByYear = (Map<Integer, Map<PaperPublishType, List<Paper>>>) SerializationHelper
				.deserialize(PAPER_CACHE_FILE);

		if (cachedPapersByYear == null) {
			synchronized (this) {
				cachedPapersByYear = (Map<Integer, Map<PaperPublishType, List<Paper>>>) SerializationHelper
						.deserialize(PAPER_CACHE_FILE);
				if (cachedPapersByYear == null) {
					// read papers
					cachedPapersByYear = readAndParseAllPapers();
					// cache papers
					SerializationHelper.serialize(cachedPapersByYear,
							PAPER_CACHE_FILE);
				}

				Assert.notNull(cachedPapersByYear);
			}
		}

		return cachedPapersByYear;
	}

	public void removeCache() {
		FileUtils.deleteQuietly(new File(PAPER_CACHE_FILE));
		System.err.println("Cache file[" + PAPER_CACHE_FILE
				+ "] has been removed.");
	}

	public static void main(String[] args) {

		// PaperCache.getInstance().addNewPapersToCache(2011);

		// Map<PaperPublishType, List<Paper>> result = PaperCache.getInstance()
		// .getPapersByPublishTypeForYear(2012);
		// System.out.println(2012);
		//
		// for (Entry<PaperPublishType, List<Paper>> papersByType : result
		// .entrySet()) {
		// for (Paper paper : papersByType.getValue()) {
		// System.out.println(papersByType.getKey() + " "
		// + paper.getMetadata().getPaperFileName());
		// }
		// }
		//
		// System.out.println("===========================================");
		//
		// List<Paper> result2 = PaperCache.getInstance().getPapers(2007,
		// PaperPublishType.LongPaper,
		// PaperPublishType.StudentWorkshopPaper);
		// for (Paper paper : result2) {
		// System.out.println(paper.getMetadata().getPaperFileName());
		// }
		//
		for (Paper paper : PaperCache.getInstance().getAllPapers()) {
			System.out.println(paper.getMetadata().getPaperFileName());
		}

	}
}
