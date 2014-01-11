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

import java.util.TreeMap;

/**
 *
 * @author Annie
 */
public class FilterInfoManager {
	
	private static final TreeMap<String, FilterInfo>filterTypes = new TreeMap<String, FilterInfo>();

	private static final FilterInfo[] filterProtos = {
		new StatelessClassFilterInfo (new BananaDetector()),
		new StatelessClassFilterInfo (new FaceDetector()),
		new StatelessClassFilterInfo (new GlassesDetector()),
		new StatelessClassFilterInfo (new ProfileDetector()),
		new StatelessClassFilterInfo (new Blur()),
		new StatelessClassFilterInfo (new ColorThreshold()),
		new StatelessClassFilterInfo (new Grayscale()),
		new StatelessClassFilterInfo (new RGBtoHSV()),
		new StatelessClassFilterInfo (new Contour()),
		new StatelessClassFilterInfo (new Dilate()),
		new StatelessClassFilterInfo (new Erode()),
		new StatelessClassFilterInfo (new Farneback())
	};
	
	static {
		for(int i = 0 ; i < filterProtos.length ; i++) {
			filterTypes.put(filterProtos[i].toString(), filterProtos[i]);
		}
	}
	
	public static FilterInfo getFilterInfo(String type) {
		return filterTypes.get(type);
	}
	
}
