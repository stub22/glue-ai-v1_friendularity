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

import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.api.west.ThingEstimate;

import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import com.jme3.asset.AssetManager;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.scene.Node;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.appdapter.core.log.BasicDebugger;

import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.model.humanoid.HumanoidFigure;
import org.cogchar.render.model.humanoid.HumanoidFigureManager;

import org.cogchar.blob.emit.RenderConfigEmitter;
import org.friendularity.api.west.WorldEstimate;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public abstract class BaseVisualizer <TE extends ThingEstimate> extends BasicDebugger implements EstimateVisualizer<TE> {
	private HumanoidRenderContext	myRenderCtx;
	
	private Map<ThingEstimate, EstimateVisualizer> mySubVizMap = new HashMap<ThingEstimate, EstimateVisualizer>();

	public BaseVisualizer(HumanoidRenderContext hrc) {
		myRenderCtx = hrc;
	}
	@Override public RenderRegistryClient getRenderRegistryClient() {
		return myRenderCtx.getRenderRegistryClient();
	}
	protected HumanoidFigureManager getHumanoidFigureManager() {
		// Not currently supplied by the RenderRegistryClient
		return myRenderCtx.getHumanoidFigureManager();
	}

	protected PhysicsSpace getPhysicsSpace() {
		// This could also be fetched through the RenderRegistryClient
		return myRenderCtx.getPhysicsSpace();
	}
	
	protected RenderConfigEmitter getConfigEmitter() { 
		return myRenderCtx.getConfigEmitter();
	}
	@Override public void renderCurrentEstimates_onRendThrd(TE estim, float timePerFrame) {
		ensureDisplayed_onRendThrd(estim, timePerFrame);
		updateDisplay_onRendThrd(estim, timePerFrame);
		renderSubEstims_onRendThrd(estim, timePerFrame);
	}	
	
	protected void renderSubEstims_onRendThrd(TE estim, float timePerFrame) { 
		Set<ThingEstimate> subEstims = estim.getSubEstimates();
		for (ThingEstimate subEstim : subEstims) {
			EstimateVisualizer subViz = getSubVisualizer(subEstim);
			if (subViz != null) {
				subViz.renderCurrentEstimates_onRendThrd(subEstim, timePerFrame);
			}
		}		
	}
	// Unsafe - we use erased type for the visualizer of the subEstimate - revisit.
	@Override public EstimateVisualizer getSubVisualizer(ThingEstimate subEstimate) {
		EstimateVisualizer subViz = mySubVizMap.get(subEstimate);
		if (subViz == null) {
			getLogger().info("Making sub-visualizer for {}", subEstimate);
			subViz = new ShapeAnimVisualizer(myRenderCtx);
			mySubVizMap.put(subEstimate, subViz);
		}
		return subViz;
	}
}
