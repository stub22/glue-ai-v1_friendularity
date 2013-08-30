/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.util;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import javax.imageio.ImageIO;

/**
 *
 * @author Annie
 */
public class OSGi_ImageLoader {
	protected static OSGi_ImageLoader theImageLoader = null;
	
	public static OSGi_ImageLoader getDefaultImageLoader()
	{
		if(theImageLoader == null)
			createDefaultImageLoader();
		
		return theImageLoader;
		
	}

	protected static void createDefaultImageLoader() {
		theImageLoader = new OSGi_ImageLoader();
	}
	
	/**
	 * Given a resource pathway gets the image
	 * 
	 * You can find the resource pathway from inside 
	 * C:\Users\Annie\.m2\repository\org\friendularity\org.friendularity.bundle.blockflow\1.0.0-SNAPSHOT
	 * or the equiv on your system
	 * 
	 * thus the duck should be "/duck.jpg"
	 * 
	 * @param resourceName  path inside jar 
	 * @return  a nice shiny BufferedImage
	 * @throws IOException if you evilly don't give us a proper path
	 */
	public BufferedImage getImageResource(String resourceName) throws IOException
	{
		return ImageIO.read(OSGi_ImageLoader.class.getResourceAsStream(resourceName));
	}
}
