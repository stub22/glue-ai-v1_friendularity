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

package org.friendularity.api.west;

import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.sys.module.RenderGateway;
import org.cogchar.render.sys.module.RenderModule;
import javax.script.ScriptEngine;
import org.cogchar.blob.emit.RenderConfigEmitter;
import org.cogchar.render.model.humanoid.HumanoidFigure;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import com.jme3.asset.AssetManager;
import com.jme3.bullet.PhysicsSpace;
import com.jme3.scene.Node;
import org.cogchar.render.model.humanoid.HumanoidFigureManager;


/**
 * @author Stu B. <www.texpedient.com>
 */

public class WorldEstimateRenderModule extends RenderModule implements WorldEstimate.Consumer  {
	private		Visualizer			myVisualizer;
	private		ScriptEngine		myCalcEngine;
	private		WorldEstimate		myCachedWorldEstim;
	public WorldEstimateRenderModule() { 
		setDebugRateModulus(1000);
	}
	@Override public void setWorldEstimate(WorldEstimate worldEstim) {
		myCachedWorldEstim = worldEstim;
	}
	@Override protected void doRenderCycle(long runSeqNum, float timePerFrame) {
		if (myCalcEngine != null) {
			// optionally do some updates+ refinements to the cached estimate, either in-place or replacing completely.
		}
		if (myVisualizer != null) {
			myVisualizer.renderCurrentEstimates(myCachedWorldEstim);
		}
	}

	// Args are all unused at present...
  	public Visualizer setupVisualizer(RepoClient rc, Ident charID, Ident vizConfGraphID) {
		RenderGateway rg = getRenderGateway();
		HumanoidRenderContext hrc = rg.getHumanoidRenderContext();
		Visualizer viz = new Visualizer(hrc);
		myVisualizer = viz;
		return viz;
	}
	protected static class Visualizer {
		private	HumanoidRenderContext	myRenderCtx;

		protected RenderRegistryClient getRenderRegistryClient() { 
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
		protected Visualizer(HumanoidRenderContext hrc) {
			myRenderCtx = hrc;
		}
		private void doMoreStuff() { 
			RenderRegistryClient rrc = getRenderRegistryClient();
			AssetManager amgr = rrc.getJme3AssetManager(null);
			Node rootNode = rrc.getJme3RootDeepNode(null);
			RenderConfigEmitter rce = myRenderCtx.getConfigEmitter();
		}
		
		protected void renderCurrentEstimates(WorldEstimate estimate) { 

		}
		
	}	
}
/*
 * BonyRenderContext brc, 
		brc.runTaskSafelyUntilComplete(new BasicCallableRenderTask(brc) {
			@Override public void performWithClient(RenderRegistryClient rrc) throws Throwable {
				boolean figureInitOK = figure.loadMeshAndSkeletonIntoVWorld(amgr, rootNode, ps);
				if (figureInitOK) {
					// Create a coroutine execution module to accept time slices, to 
					// allows us to animate the humanoid figure.
					final HumanoidFigureModule hfm = new HumanoidFigureModule(figure, brc);
 */