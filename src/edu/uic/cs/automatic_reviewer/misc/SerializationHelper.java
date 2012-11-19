package edu.uic.cs.automatic_reviewer.misc;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

public class SerializationHelper {

	public static void serialize(Object object, String filePath) {

		Assert.isTrue(object instanceof Serializable, "Object [" + object
				+ "] must implements Serializable interface.");

		FileUtils.deleteQuietly(new File(filePath));

		ObjectOutputStream oos = null;
		try {
			BufferedOutputStream stream = new BufferedOutputStream(
					new FileOutputStream(filePath));
			oos = new ObjectOutputStream(stream);
			oos.writeObject(object);
			oos.flush();
		} catch (Exception e) {
			System.err.println("Fail to serialize [" + filePath
					+ "] into hard disk! ");
			throw new AutomaticReviewerException(e);
		} finally {
			IOUtils.closeQuietly(oos);
		}
	}

	public static Object deserialize(String filePath) {
		ObjectInputStream ois = null;
		try {
			BufferedInputStream stream = new BufferedInputStream(
					new FileInputStream(filePath));
			ois = new ObjectInputStream(stream);

			Object cached = ois.readObject();
			return cached;

		} catch (Exception exception) {
			System.err.println("No serialized object exists for [" + filePath
					+ "]. ");
			return null;
		} finally {
			IOUtils.closeQuietly(ois);
		}
	}
}
