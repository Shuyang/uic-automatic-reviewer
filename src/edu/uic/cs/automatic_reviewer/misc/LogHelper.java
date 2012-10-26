package edu.uic.cs.automatic_reviewer.misc;

import org.apache.log4j.Logger;

public class LogHelper
{
	public static final String LOG_LAYER_THREE = "********* ";
	public static final String LOG_LAYER_THREE_BEGIN = "*********BEGIN*** ";
	public static final String LOG_LAYER_THREE_END = "*********END***** ";

	public static final String LOG_LAYER_TWO = "******";
	public static final String LOG_LAYER_TWO_BEGIN = "******BEGIN*** ";
	public static final String LOG_LAYER_TWO_END = "******END***** ";

	public static final String LOG_LAYER_ONE = "***";
	public static final String LOG_LAYER_ONE_BEGIN = "***BEGIN*** ";
	public static final String LOG_LAYER_ONE_END = "***END***** ";

	public static Logger getLogger(Class<?> clazz)
	{
		return Logger.getLogger(clazz);
	}
}
