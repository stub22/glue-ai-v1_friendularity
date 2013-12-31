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
 *
 * @author Annie
 */
public class ImageStreamImage {
	Mat	asOpenCV = null;
	BufferedImage asBufferedImage = null;
	
	public ImageStreamImage(Mat mat) {
		asOpenCV = mat;
	}
	
	public ImageStreamImage(BufferedImage buf) {
		asBufferedImage = buf;
	}
	
	public Mat getMat() throws ImageFlavorNotAvailable {
		if (asOpenCV == null && asBufferedImage == null)
			throw new ImageFlavorNotAvailable();
		
		if (asOpenCV == null) {
			// TODO write this conversion when you need it
			throw new ImageFlavorNotAvailable();
		}
		
		return asOpenCV;
	}
	
	public BufferedImage getBufferedImage() throws ImageFlavorNotAvailable {
		if (asOpenCV == null && asBufferedImage == null)
			throw new ImageFlavorNotAvailable();
		
		if (asBufferedImage == null) {
			asBufferedImage = JVisionEngine.matToBufferedImage(asOpenCV);
		}
		
		return asBufferedImage;		
	}
	
}
