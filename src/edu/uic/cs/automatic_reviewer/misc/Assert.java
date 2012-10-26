package edu.uic.cs.automatic_reviewer.misc;

import java.util.Collection;
import java.util.Map;

public class Assert
{
	private static final String EXCEPTION_MESSAGE_NOT_NULL = "Parameter can not be null. ";
	private static final String EXCEPTION_MESSAGE_IS_TRUE = "Condition must be [true]. ";
	private static final String EXCEPTION_MESSAGE_IS_NULL = "Parameter must be null. ";
	private static final String EXCEPTION_MESSAGE_NOT_EMPTY = "Parameter can not be empty. ";

	public static void isNull(Object obj)
	{
		if (obj != null)
		{
			throw new AutomaticReviewerException(EXCEPTION_MESSAGE_IS_NULL);
		}
	}

	public static void notNull(Object obj)
	{
		notNull(obj, null);
	}

	public static void notNull(Object obj, String message)
	{
		if (obj == null)
		{
			throw (message == null) ? new AutomaticReviewerException(
					EXCEPTION_MESSAGE_NOT_NULL) : new AutomaticReviewerException(
					EXCEPTION_MESSAGE_NOT_NULL + "[Message]" + message);
		}
	}

	public static void notEmpty(Object obj)
	{
		notEmpty(obj, null);
	}

	public static void notEmpty(Object obj, String message)
	{
		notNull(obj, message);
		boolean isEmptyString = obj instanceof String
				&& ((String) obj).trim().length() == 0;
		boolean isEmptyCollection = obj instanceof Collection<?>
				&& ((Collection<?>) obj).isEmpty();
		boolean isEmptyMap = obj instanceof Map<?, ?>
				&& ((Map<?, ?>) obj).isEmpty();
		if (isEmptyString || isEmptyCollection || isEmptyMap)
		{
			throw (message == null) ? new AutomaticReviewerException(
					EXCEPTION_MESSAGE_NOT_EMPTY) : new AutomaticReviewerException(
					EXCEPTION_MESSAGE_NOT_EMPTY + "[Message]" + message);
		}
	}

	public static void isTrue(boolean condition)
	{
		isTrue(condition, null);
	}

	public static void isTrue(boolean condition, String message)
	{
		if (!condition)
		{
			throw (message == null) ? new AutomaticReviewerException(
					EXCEPTION_MESSAGE_IS_TRUE) : new AutomaticReviewerException(
					EXCEPTION_MESSAGE_IS_TRUE + "[Message]" + message);
		}
	}
}
