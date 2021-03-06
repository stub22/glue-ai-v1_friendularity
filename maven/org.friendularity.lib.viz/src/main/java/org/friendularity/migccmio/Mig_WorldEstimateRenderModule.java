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
package org.friendularity.migccmio;

import com.jme3.scene.Node;
import org.appdapter.core.name.Ident;
import org.appdapter.fancy.rclient.RepoClient;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.sys.module.RenderGateway;
import org.cogchar.render.sys.module.RenderModule;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.api.west.WorldEstimate;
import org.friendularity.impl.visual.DemoWorldVisualizer;
import org.friendularity.impl.visual.EstimateVisualizer;
import org.friendularity.visual.texture.JVisionTextureMapper;
import org.friendularity.visual.texture.MagicVisionBoxScene;
// import org.friendularity.vsim.vworld.SnapshotMonitor;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class Mig_WorldEstimateRenderModule extends RenderModule implements WorldEstimate.Consumer {

	private EstimateVisualizer<WorldEstimate> myWorldEstimVisualizer;
	// calc engine holds a set of variables defining our state.
	// we treat these variables as RDF:Nodes.  Our dog's position, in some environment coord frame, rep in turtle as:
	// dog_pos inFrame env_frame_18; hasX 22.2 ; hasY 0.3; hasZ  -14.0.
	// Tricky question:  At a higher logical level, how do we maintain symbol table correspondence?
	// (Answer:  ontologized canonical axes)
	// Symja is function-oriented, and does not obviously/trivially supply an associate-array or record construct.
	private MathGate myMathGate;
	private WorldEstimate myCachedWorldEstim;
	boolean myFlag_bonusMeshesNeeded = true;  // BonusMeshes includes MagicVisionBoxScene

	private boolean myFlag_JVisionTextureRoutingEnabled = false;

	private MagicVisionBoxScene		myMVBS;
	private SnapMonApi			mySnapMon;  // extends TrialContent, includes 3D gridspace of numbers+colors
	private Mig_DemoMidiCommandMapper myMidiMapper;
	
	public Mig_WorldEstimateRenderModule() {
		setDebugRateModulus(1000);
	}

	@Override public void setWorldEstimate(WorldEstimate worldEstim) {
		myCachedWorldEstim = worldEstim;
	}
	public void setMidiMapper(Mig_DemoMidiCommandMapper midiMapper) {
		myMidiMapper = midiMapper;
	}

	public WorldEstimate getWorldEstimate() {
		return myCachedWorldEstim;
	}

	public void setMathGate(MathGate mg) {
		myMathGate = mg;
	}

	public void setFlag_JVisionTextureRoutingEnabled(boolean flag) {
		myFlag_JVisionTextureRoutingEnabled = flag;
	}

	private JVisionTextureMapper setupJVisionConnection() {
		getLogger().info("Setup for vision-texture-mappper");
		JVisionTextureMapper jvtm = new JVisionTextureMapper();
		jvtm.connectToImageStreamBroker(); 
		return jvtm;
	}
	private void setupMagicVisionBoxScene_onRendThrd(RenderRegistryClient rrc, JVisionTextureMapper optJVTM, float tpf) {
		// Currently called from doRenderCycle() below.
		getLogger().info("One time setup for Magic Vision Box Scene");
		myMVBS = new MagicVisionBoxScene();
		myMVBS.setup_onRendThrd(rrc, tpf);
		if (optJVTM != null) {
			myMVBS.setJVisionTextureMapper(optJVTM);  // MVBS will now poll JVTM for updated vision textures
		}

	}
	abstract protected SnapMonApi makeSnapMon();  // mySnapMon = new SnapshotMonitor(); // extends TrialContent, shows big matrix of numbers+colors

	protected void setupSnapMon(RenderRegistryClient rrc, JVisionTextureMapper optJVTM) {
		Node rootDeepNode = rrc.getJme3RootDeepNode(null);
		mySnapMon = makeSnapMon();
		mySnapMon.setup_onRendThrd(rrc, rootDeepNode);
		mySnapMon.setJVisionTextureMapper(optJVTM);  // OK to set it to null
		if ((myMidiMapper != null) && (myMidiMapper.myCCPR != null)) {
			mySnapMon.attachMidiCCs(myMidiMapper.myCCPR);
		} else {
			getLogger().warn("NOT setting up CC-paramRouter mapping to SnapshotMonitor!  midiMapper={}, ccpr={}",
					myMidiMapper, (myMidiMapper != null) ? myMidiMapper.myCCPR : null);
		}

	}
	@Override protected void doRenderCycle(long runSeqNum, float timePerFrame) {

		if (myMathGate != null) {
			// optionally do some updates+ refinements to the cached estimate, either in-place or replacing completely.
			if (myCachedWorldEstim != null) {
				// Using *unscripted* MathGate avoids serious memory leak in Symja's script-wrapper.
				myCachedWorldEstim.updateFromMathSpace(myMathGate);
			}
		}

		if (myWorldEstimVisualizer != null) {
			if (myCachedWorldEstim != null) {
				myWorldEstimVisualizer.renderCurrentEstimates_onRendThrd(myCachedWorldEstim, timePerFrame);
			}

			if (myFlag_bonusMeshesNeeded) {
				myFlag_bonusMeshesNeeded = false;
				JVisionTextureMapper optJVTM = null;
				if (myFlag_JVisionTextureRoutingEnabled) {
					optJVTM = setupJVisionConnection(); 
				}
				getLogger().info("One time setup for bonus-meshes");
				RenderRegistryClient rrc = myWorldEstimVisualizer.getRenderRegistryClient();
			
				((DemoWorldVisualizer) myWorldEstimVisualizer).makeBonusMeshes();
				
				if (mySnapMon == null) {
					setupSnapMon(rrc, optJVTM);
				}
				if (myMVBS == null) {
					// Disable this line if we get sceneGraph concurrency errors mentioning "offBox"
					setupMagicVisionBoxScene_onRendThrd(rrc, optJVTM, timePerFrame);
				}					
			
			} else {
				// We do not want these updates to happen on the same (one-time) loop as the bonus-mesh init above.
				// Hence the "else" clause we are in.
				
				if (mySnapMon != null) {
					// uses vision-texture "peek"
					mySnapMon.update_onRendThrd(timePerFrame);
				}

				if (myMVBS != null) {
					// uses vision-texture "take"
					myMVBS.update_onRendThrd(timePerFrame);	// Includes polling for new vision textures
				}
			}
		}

	}
	// TODO:  Use these helpful config args to set up the visualization pipeline.

	public EstimateVisualizer setupVisualizer(RepoClient rc, Ident charID, Ident vizConfGraphID) {
		getLogger().info("Setting up visualizer for char {} with confGraph {}", charID, vizConfGraphID);

		RenderGateway rg = getRenderGateway();
		HumanoidRenderContext hrc = (HumanoidRenderContext) rg.getCogcharRenderContext();
		EstimateVisualizer viz = new DemoWorldVisualizer(hrc);
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