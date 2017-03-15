/*
*  Copyright 2013 by The Friendularity Project (www.friendularity.org).
*
*  Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*  See the License for the specific language governing permissions and
*  limitations under the License.
*/
package org.friendularity.bundle.bento.util;

/**
 * This class is a virtual copy of the one in Blockview
 * this is evil. I'm reluctant to suck in all of Blockview just for this
 *
 * @author Annie
 */

import com.hp.hpl.jena.rdf.model.Model;

import org.friendularity.bundle.bento.BentoBundleActivator;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * @author Annie
 */
public class Bento_OSGi_ResourceLoader {
	protected static Bento_OSGi_ResourceLoader theImageLoader = null;

	public static Bento_OSGi_ResourceLoader getDefaultImageLoader() {
		if (theImageLoader == null)
			createDefaultImageLoader();

		return theImageLoader;

	}

	protected static void createDefaultImageLoader() {
		theImageLoader = new Bento_OSGi_ResourceLoader();
	}

	/**
	 * Given a resource pathway gets the image
	 *
	 * You can find the resource pathway from inside
	 * C:\Users\Annie\.m2\repository\org\friendularity\org.friendularity.bundle.bento\1.0.0-SNAPSHOT
	 * or the equiv on your system
	 *
	 * thus the duck should be "/duck.jpg"
	 *
	 * @param resourceName path inside jar
	 * @return a nice shiny BufferedImage
	 * @throws IOException if you evilly don't give us a proper path
	 */
	public BufferedImage getImageResource(String resourceName) throws IOException {
		return ImageIO.read(Bento_OSGi_ResourceLoader.class.getResourceAsStream(resourceName));
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
			Image img = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource(resourceName);
			return toolkit.createCustomCursor(img, new Point(x, y), name);
		} catch (IOException ex) {
			LoggerFactory.getLogger(BentoBundleActivator.class).error(ex.getMessage(), ex);
			return Cursor.getDefaultCursor();
		}
	}

	/**
	 * load a
	 */
	public void loadModelFromTurtleResource(Model model, String resourceName) {
		model.read(Bento_OSGi_ResourceLoader.class.getResourceAsStream(resourceName), null, "TURTLE");
	}
}
