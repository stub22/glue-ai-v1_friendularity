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
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;

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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Detector"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Detector"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Detector"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Detector"};
				return c;
			}
		},
		// ======== image processing ===========
		new FilterInfo() {
			@Override
			public String toString() {
				return "color threshold"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new ColorThreshold();
			}

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "ImageProc"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "ImageProc"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "ImageProc"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Line Operations"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Morphology"};
				return c;
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

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Morphology"};
				return c;
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "Farneback optical flow visual"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new Farneback();
			}

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Motion"};
				return c;
			}
		},
		new FilterInfo() {
			@Override
			public String toString() {
				return "Farneback optical flow BGx"; 
			}

			@Override
			public BaseFilter createInstance() {
				return new FarnebackBGx();
			}

			@Override
			public String[] getCategory() {
				String[] c = {"CV", "Motion"};
				return c;
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

	public static void addAllFilterInfoToTree(DefaultMutableTreeNode top) {
		
        DefaultMutableTreeNode category = null;
        DefaultMutableTreeNode filter = null;
		
		for(int i = 0 ; i < filterProtos.length ; i++) {
			addAFilter(top, 0 , filterProtos[i].getCategory(), filterProtos[i]);
		}

	}

	private static void addAFilter(TreeNode node, int index, String[] category, FilterInfo filterInfo) {
		if(index >= category.length) {  // add the filterInfo
			// if the node exists, it's probably a bad thing, but we do what we're told and add it
			((DefaultMutableTreeNode)node).add(new DefaultMutableTreeNode(filterInfo , false));
		} else {  // add a node recursively
			for(int i = 0 ; i < node.getChildCount() ; i++) {
				if(node.getChildAt(i).toString().equals(category[index])) {
					addAFilter(node.getChildAt(i), index + 1, category, filterInfo);
					return;
				}
			}
			// this node doesn't exist yet, make it
			DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(category[index]);
			((DefaultMutableTreeNode)node).add(newnode);
			addAFilter(newnode, index + 1, category, filterInfo);
		}
	}
}
