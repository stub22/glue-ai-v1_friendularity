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
		new FilterInfo() {
			@Override
			public String toString() {
				return "banana detector"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new BananaDetector();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "face detector"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new FaceDetector();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "glasses detector"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new GlassesDetector();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "profile detector"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new ProfileDetector();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "color threshold"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new ColorThreshold();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "grey"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new Grayscale();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "BGR to HSV"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new RGBtoHSV();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "contour display"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new Contour();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "dilate"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new Dilate();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "erode"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new Erode();
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "Farneback optical flow"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new Farneback();
			}
		}
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
