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

import javax.swing.JFrame;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

/**
 *
 * @author Annie
 */
public class QuarterSize implements BaseFilter {

	@Override
	public void apply(Mat in, Mat out) {
		 
		Imgproc.resize(in, out, new Size(in.width() /4.0, in.height() / 4.0));
	}

	@Override
	public void showParamUI(JFrame parent) {
		
	}

	@Override
	public String toString() {
		return "quartersize";
	}

	@Override
	public String serialize() {
		return "";
	}

	@Override
	public void deserialize(String str) {
		
	}
	
}
