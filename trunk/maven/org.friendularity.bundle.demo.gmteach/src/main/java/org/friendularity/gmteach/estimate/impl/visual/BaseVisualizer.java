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

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.blob.emit.RenderConfigEmitter;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.model.humanoid.HumanoidFigureManager;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.gmteach.estimate.api.west.ThingEstimate;

import com.jme3.bullet.PhysicsSpace;

/**
 * 
 * @author Stu B22 <stub22@appstract.com>
 */
public abstract class BaseVisualizer<TE extends ThingEstimate> extends BasicDebugger implements EstimateVisualizer<TE> {
	private HumanoidRenderContext myRenderCtx;

	public BaseVisualizer(HumanoidRenderContext hrc) {
		myRenderCtx = hrc;
	}

	public BaseVisualizer(BaseVisualizer<?> otherViz) {
		myRenderCtx = otherViz.myRenderCtx;
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

	@Override final public void renderCurrentEstimates_onRendThrd(TE estim, float timePerFrame) {
		ensureDisplayed_onRendThrd(estim, timePerFrame);
		updateDisplay_onRendThrd(estim, timePerFrame);
	}

	protected abstract void ensureDisplayed_onRendThrd(TE te, float timePerFrame);

	protected abstract void updateDisplay_onRendThrd(TE te, float timePerFrame);
}
