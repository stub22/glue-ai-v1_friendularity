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
package org.friendularity.jvision.broker;

import java.awt.image.BufferedImage;
import org.friendularity.jvision.engine.JVisionEngine;
import org.opencv.core.Mat;

/**
 * A multi-flavored 'image' delivered by an image stream.
 * ImageStreamConsumer s will receive a series of these.
 * 
 * the payload can be extracted from any of the getFoo methods. If ImageStreamImage knows
 * how to convert it's payload to that type, it will. If not, it will throw ImageFlavorNotAvailable.
 * 
 * An exception to this behavior is Object. Packets created with the Object constructor will only return their
 * contents via getObject.
 * 
 * @author Annie
 */
public class ImageStreamImage {
	private Mat	asOpenCV = null;
	private BufferedImage asBufferedImage = null;
	private Object asObject = null;
	
	public ImageStreamImage(Mat mat) {
		asOpenCV = mat;
	}
	
	public ImageStreamImage(BufferedImage buf) {
		asBufferedImage = buf;
	}
	
	public ImageStreamImage(Object obj) {
		asObject = obj;
	}
	
	/**
	 * 
	 * @return an Object. This ImageStreamImage must have been created with the 
	 * Object argument constructor. While of course any type can be converted to Object,
	 * the assumption here is that the consumer and producer are otherwise agreeing on the
	 * class of the object and that the consumer is prepared to check the class and act
	 * appropriately. So it's more convenient for the caller for this to treat images as special and 
	 * not return them here.
	 * 
	 * @throws ImageFlavorNotAvailable 
	 */
	public Object getObject() throws ImageFlavorNotAvailable {
		if (asObject == null)throw new ImageFlavorNotAvailable();
		
		return asObject;
	}
	
	/**
	 * 
	 * @return the image as an OpenCV Mat type.
	 * 
	 * @throws ImageFlavorNotAvailable 
	 * 
	 * @tbd this should convert BufferedImage to Mat when needed, but this isn't implemented
	 * callers should not depend on this throwing in such circumstances, some day it will be fixed
	 * 
	 */
	public Mat getMat() throws ImageFlavorNotAvailable {
		if (asOpenCV == null && asBufferedImage == null)
			throw new ImageFlavorNotAvailable();
		
		if (asOpenCV == null) {
			// TODO write this conversion when you need it
			throw new ImageFlavorNotAvailable();
		}
		
		return asOpenCV;
	}

	/**
	 * 
	 * @return the image as a BufferedImage
	 * 
	 * @throws ImageFlavorNotAvailable 
	 * 
	 * This will allocate a buffer and convert a Mat if necessary.
	 * 
	 */
	public BufferedImage getBufferedImage() throws ImageFlavorNotAvailable {
		if (asOpenCV == null && asBufferedImage == null)
			throw new ImageFlavorNotAvailable();
		
		if (asBufferedImage == null) {
			asBufferedImage = JVisionEngine.matToBufferedImage(asOpenCV);
		}
		
		return asBufferedImage;		
	}
	
}
