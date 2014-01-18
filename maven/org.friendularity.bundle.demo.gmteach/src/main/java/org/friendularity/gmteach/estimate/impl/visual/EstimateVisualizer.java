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
package org.friendularity.gmteach.estimate.impl.visual;

import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.cogchar.render.goody.dynamic.VizShapeGroup;
import org.friendularity.gmteach.estimate.api.west.ThingEstimate;


/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public interface EstimateVisualizer<TE extends ThingEstimate> {
	public RenderRegistryClient getRenderRegistryClient();
	
	/**
	 * This shapeGroup may be either local to this visualizer, or shared with others.
	 * @return 
	 */
	public VizShapeGroup getShapeGroup();
	
	public void renderCurrentEstimates_onRendThrd(TE estim, float timePerFrame);

	
	
	public static class Jme3CoordinateFrame extends ThingEstimate.CoordinateFrame {
		// Vector loc + Quat rot
	}	
}
