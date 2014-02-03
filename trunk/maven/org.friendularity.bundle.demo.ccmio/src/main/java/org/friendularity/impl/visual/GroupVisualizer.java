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

package org.friendularity.impl.visual;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.friendularity.api.west.ThingEstimate;
import org.friendularity.api.west.CompoundEstimate;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.goody.dynamic.VizShapeGroup;

/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class GroupVisualizer <CTE extends CompoundEstimate> extends BaseVisualizer<CTE> {
	
	private Map<ThingEstimate, EstimateVisualizer> mySubVizMap = new HashMap<ThingEstimate, EstimateVisualizer>();
	
	public GroupVisualizer(HumanoidRenderContext hrc) {
		super(hrc);
	}	
	@Override protected void  ensureDisplayed_onRendThrd(CTE estim, float timePerFrame) {
		Set<ThingEstimate> subEstims = estim.getSubEstimates();
		for (ThingEstimate subEstim : subEstims) {
			EstimateVisualizer subViz = getSubVisualizer(subEstim);
			if (subViz != null) {
				if (subViz instanceof BaseVisualizer) {
					((BaseVisualizer) subViz).ensureDisplayed_onRendThrd(subEstim, timePerFrame);
				}
			}
		}			
	}
	@Override protected void  updateDisplay_onRendThrd(CTE estim, float timePerFrame) {
		Set<ThingEstimate> subEstims = estim.getSubEstimates();
		for (ThingEstimate subEstim : subEstims) {
			EstimateVisualizer subViz = getSubVisualizer(subEstim);
			if (subViz != null) {
				if (subViz instanceof BaseVisualizer) {
					((BaseVisualizer) subViz).updateDisplay_onRendThrd(subEstim, timePerFrame);
				}
			}
		}			
	}
	// Unsafe - we use erased type for the visualizer of the subEstimate - revisit.
	public EstimateVisualizer getSubVisualizer(ThingEstimate subEstimate) {
		EstimateVisualizer subViz = mySubVizMap.get(subEstimate);
		if (subViz == null) {
			getLogger().info("Making sub-visualizer for {}", subEstimate);
			VizShapeGroup existingGroup = getShapeGroup();
			
			subViz = new SingleShapeVisualizer(this, existingGroup);
			mySubVizMap.put(subEstimate, subViz);
		}
		return subViz;
	}
}
