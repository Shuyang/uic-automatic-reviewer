package edu.uic.cs.automatic_reviewer.input;

import java.util.Map;

public class Metadata {

	private String creatorTool;

	private Map<Integer, Integer> numOfFiguresByPage;
	private int numOfFigures = 0;
	private int maxNumOfFiguresOnOnePage = 0;
	private int numOfPagesHaveFigure = 0;

	private Map<Integer, Integer> numOfTablesByPage;
	private int numOfTables;
	private int maxNumOfTablesOnOnePage = 0;
	private int numOfPagesHaveTable = 0;

	private Map<Integer, Integer> numOfFormulasByPage;
	private int numOfFormulas;
	private int maxNumOfFormulasOnOnePage = 0;
	private int numOfPagesHaveFormula = 0;

	public void setNumOfFiguresByPage(Map<Integer, Integer> countByPage) {

		this.numOfFiguresByPage = countByPage;

		for (Integer numOfFigures : countByPage.values()) {
			this.numOfFigures += numOfFigures;

			if (maxNumOfFiguresOnOnePage < numOfFigures.intValue()) {
				maxNumOfFiguresOnOnePage = numOfFigures.intValue();
			}

			if (numOfFigures.intValue() > 0) {
				numOfPagesHaveFigure++;
			}
		}
	}

	public void setNumOfTablesByPage(Map<Integer, Integer> numOfTablesByPage) {
		this.numOfTablesByPage = numOfTablesByPage;

		for (Integer numOfTables : numOfTablesByPage.values()) {
			this.numOfTables += numOfTables;

			if (maxNumOfTablesOnOnePage < numOfTables.intValue()) {
				maxNumOfTablesOnOnePage = numOfTables.intValue();
			}

			if (numOfTables.intValue() > 0) {
				numOfPagesHaveTable++;
			}
		}
	}

	public void setNumOfFormulasByPage(Map<Integer, Integer> numOfFormulasByPage) {
		this.numOfFormulasByPage = numOfFormulasByPage;

		for (Integer numOfFormulas : numOfFormulasByPage.values()) {
			this.numOfFormulas += numOfFormulas;

			if (maxNumOfFormulasOnOnePage < numOfFormulas.intValue()) {
				maxNumOfFormulasOnOnePage = numOfFormulas.intValue();
			}

			if (numOfFormulas.intValue() > 0) {
				numOfPagesHaveFormula++;
			}
		}
	}

	public Map<Integer, Integer> getNumOfFormulasByPage() {
		return numOfFormulasByPage;
	}

	public int getNumOfFormulas() {
		return numOfFormulas;
	}

	public int getMaxNumOfFormulasOnOnePage() {
		return maxNumOfFormulasOnOnePage;
	}

	public int getNumOfPagesHaveFormula() {
		return numOfPagesHaveFormula;
	}

	public Map<Integer, Integer> getNumOfFiguresByPage() {
		return numOfFiguresByPage;
	}

	public Map<Integer, Integer> getNumOfTablesByPage() {
		return numOfTablesByPage;
	}

	public int getNumOfFigures() {
		return numOfFigures;
	}

	public int getMaxNumOfFiguresOnOnePage() {
		return maxNumOfFiguresOnOnePage;
	}

	public int getNumOfPagesHaveFigure() {
		return numOfPagesHaveFigure;
	}

	public int getNumOfTables() {
		return numOfTables;
	}

	public int getMaxNumOfTablesOnOnePage() {
		return maxNumOfTablesOnOnePage;
	}

	public int getNumOfPagesHaveTable() {
		return numOfPagesHaveTable;
	}

}
