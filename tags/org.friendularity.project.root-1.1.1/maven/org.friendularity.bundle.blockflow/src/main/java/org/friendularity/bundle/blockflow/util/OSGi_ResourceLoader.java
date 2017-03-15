/**
 * This is the missing license file
 */
package org.friendularity.bundle.blockflow.util;

import com.hp.hpl.jena.rdf.model.Model;

import org.friendularity.bundle.blockflow.gui.BlockViewportController;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.imageio.ImageIO;


/**
 * @author Annie
 */
public class OSGi_ResourceLoader {
	protected static OSGi_ResourceLoader theImageLoader = null;

	public static OSGi_ResourceLoader getDefaultImageLoader() {
		if (theImageLoader == null)
			createDefaultImageLoader();

		return theImageLoader;

	}

	protected static void createDefaultImageLoader() {
		theImageLoader = new OSGi_ResourceLoader();
	}

	/**
	 * Given a resource pathway gets the image
	 *
	 * You can find the resource pathway from inside C:\Users\Annie\.m2\repository\org\friendularity\org.friendularity.bundle.blockflow\1.0.0-SNAPSHOT
	 * or the equiv on your system
	 *
	 * thus the duck should be "/duck.jpg"
	 *
	 * @param resourceName path inside jar
	 * @return a nice shiny BufferedImage
	 * @throws IOException if you evilly don't give us a proper path
	 */
	public BufferedImage getImageResource(String resourceName) throws IOException {
		return ImageIO.read(OSGi_ResourceLoader.class.getResourceAsStream(resourceName));
	}

	/**
	 * Get a cursor as a resource
	 *
	 * @param resourceName name of the resource within the bundle
	 * @param x            hotspot x
	 * @param y            hotspot y
	 * @param name         cursor name (could be anything)
	 * @return A shiny new Cursor
	 */
	public Cursor getCursorResource(String resourceName, int x, int y, String name) {
		Toolkit toolkit = Toolkit.getDefaultToolkit();

		try {
			Image img = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource(resourceName);
			return toolkit.createCustomCursor(img, new Point(x, y), name);
		} catch (IOException ex) {
			LoggerFactory.getLogger(BlockViewportController.class).error(ex.getMessage(), ex);
			return Cursor.getDefaultCursor();
		}
	}

	/**
	 * load a
	 */
	public void loadModelFromTurtleResource(Model model, String resourceName) {
		model.read(OSGi_ResourceLoader.class.getResourceAsStream(resourceName), null, "TURTLE");
	}
}
