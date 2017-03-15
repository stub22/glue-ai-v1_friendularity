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
package org.friendularity.jvision.filters;

import org.friendularity.jvision.broker.ImageFlavorNotAvailable;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.opencv.core.Mat;
import org.slf4j.LoggerFactory;

/**
 * Base class for filters that take one Mat and produce another
 *
 * @author Annie
 */
public abstract class ApplicativeBaseFilter extends BaseFilter {


	public ApplicativeBaseFilter() {
		super();
	}

	@Override
	public String getSourceName() {
		return "Anonymous applicative filter.";
	}

	@Override
	public void setConsumedImage(ImageStreamImage img) {
		try {
			Mat out = new Mat();

			apply(img.getMat(), out);
			isp.setConsumedImage(new ImageStreamImage(out));
		} catch (ImageFlavorNotAvailable ex) {
			LoggerFactory.getLogger(ApplicativeBaseFilter.class).error(ex.getMessage(), ex);
		}

	}

	public abstract void apply(Mat in, Mat out);
}
