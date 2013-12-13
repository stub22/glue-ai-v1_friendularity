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
package org.friendularity.gmteach.api.west;

import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.sys.module.RenderGateway;
import org.cogchar.render.sys.module.RenderModule;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.gmteach.impl.visual.BonusVisualizer;
import org.friendularity.gmteach.vworld.VisionTextureMapper;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class WorldEstimateRenderModule extends RenderModule implements
		WorldEstimate.Consumer {

	private EstimateVisualizer<WorldEstimate> myWorldEstimVisualizer;
	// calc engine holds a set of variables defining our state.
	// we treat these variables as RDF:Nodes.  Our dog's position, in some environment coord frame, rep in turtle as:
	// dog_pos inFrame env_frame_18; hasX 22.2 ; hasY 0.3; hasZ  -14.0.
	// Tricky part:  How do we maintain symbol table correspondence?
	// Symja is function-oriented, and does not obviously/trivially supply an associate-array or record construct.
	private MathGate myMathGate;
	private WorldEstimate myCachedWorldEstim;
	boolean myDidThatStuffFlag = false;
	private VisionTextureMapper myVTM;
	private boolean myFlag_visionTextureRoutingEnabled = false;

	public WorldEstimateRenderModule() {
		setDebugRateModulus(1000);
	}

	@Override
	public void setWorldEstimate(WorldEstimate worldEstim) {
		myCachedWorldEstim = worldEstim;
	}

	public WorldEstimate getWorldEstimate() {
		return myCachedWorldEstim;
	}

	public void setMathGate(MathGate mg) {
		myMathGate = mg;
	}

	public void setFlag_visionTextureRoutingEnabled(boolean flag) {
		myFlag_visionTextureRoutingEnabled = flag;
	}

	@Override
	protected void doRenderCycle(long runSeqNum, float timePerFrame) {

		if (myMathGate != null) {
			// optionally do some updates+ refinements to the cached estimate, either in-place or replacing completely.
			if (myCachedWorldEstim != null) {
				// 2013-08-01:  Doing just this step alone (with rendering disabled below) 
				// is enough to leak 2+G of RAM in 15 min [projectile shooting was also enabled in that trial,
				// but it doesn't leak this fast on its own]

				myCachedWorldEstim.updateFromMathSpace(myMathGate);
			}
		}

		if (myWorldEstimVisualizer != null) {
			if (myCachedWorldEstim != null) {
				myWorldEstimVisualizer.renderCurrentEstimates(
						myCachedWorldEstim, timePerFrame);
			}

			if (!myDidThatStuffFlag) {
				getLogger().info("One time setup for bonus-meshes");
				myDidThatStuffFlag = true;
				((BonusVisualizer) myWorldEstimVisualizer).makeBonusMeshes();
			}
			if (myFlag_visionTextureRoutingEnabled) {
				if (myVTM == null) {
					getLogger().info(
							"One time setup for vision-texture-mappper");
					myVTM = new VisionTextureMapper();
					RenderRegistryClient rrc = myWorldEstimVisualizer
							.getRenderRegistryClient();
					myVTM.setup(rrc);
				}
				myVTM.simpleUpdate(timePerFrame);
			}
		}

	}

	// TODO:  Use these helpful config args to set up the visualization pipeline.

	public EstimateVisualizer setupVisualizer(RepoClient rc, Ident charID,
			Ident vizConfGraphID) {
		getLogger().info("Setting up visualizer for char {} with confGraph {}",
				charID, vizConfGraphID);

		RenderGateway rg = getRenderGateway();
		HumanoidRenderContext hrc = (HumanoidRenderContext) rg
				.getCogcharRenderContext();
		EstimateVisualizer viz = new BonusVisualizer(hrc);
		myWorldEstimVisualizer = viz;
		return viz;
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