/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

package org.cogchar.app.puma.behavior;

import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.app.puma.config.PumaConfigManager;
import org.cogchar.app.puma.registry.PumaRegistryClient;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.impl.scene.Theater;
import org.cogchar.impl.scene.TheaterTest;
import org.cogchar.platform.trigger.CogcharScreenBox;
//import org.cogchar.render.app.trigger.SceneActions;
import org.osgi.framework.BundleContext;
import org.cogchar.platform.trigger.BoxSpace;

/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class PumaBehaviorAgent extends CogcharScreenBox {
	
	protected		Ident						myAgentID;
	protected		BehaviorConfigEmitter		myBehaviorCE;
	public			Theater						myTheater;
	
	private		static		BundleContext	BUNCTX_FOR_CRUDE_RESTART = null;
		
	public PumaBehaviorAgent(Ident agentID, BehaviorConfigEmitter bce)  {
		myAgentID = agentID;
		myBehaviorCE = bce;
		myTheater = new Theater(myAgentID);	
	}
	protected Ident getAgentID() {
		return myAgentID;
	}
	

	public void setupAndStart(BundleContext bunCtx, PumaRegistryClient prc, String chanGraphQN,  String behavGraphQN)  { 
		try {
			PumaConfigManager  pcm = prc.getConfigMgr(null);
			setupAndStartBehaviorTheater(bunCtx, pcm, chanGraphQN,  behavGraphQN);
			// We connect animation output channels for triggering (regardless of whether we are doing virtual-world animation or not).
			// Hard-wired local channels were once crudely setup right here.  See doWiringPostStart().
			BoxSpace bs = prc.getTargetBoxSpace(null);
			bs.addBox(getCharIdent(), this);
		} catch (Throwable t) {
			getLogger().error("Cannot setup+start behavior theater", t);
		}
	}
	

	public void setupAndStartBehaviorTheater(BundleContext bunCtx, PumaConfigManager pcm, String chanGraphQN, String behavGraphQN) throws Throwable {
		boolean clearCachesFirst = true;
		// Old way, may still be useful durnig behavior development by advanced users.
		// loadBehaviorConfigFromTestFile(clearCachesFirst);
		RepoClient rc = pcm.getMainConfigRepoClient();
		Ident	chanGraphID = rc.makeIdentForQName(chanGraphQN);
		
		Ident	behavGraphID = rc.makeIdentForQName(behavGraphQN);
		loadBehaviorConfigFromRepo(rc, chanGraphID, behavGraphID, clearCachesFirst);
		
		startTheater(bunCtx);
	}	

	public void startTheater(BundleContext optBunCtxForWiring) {
//		doWiringPreStart(optBunCtxForWiring, myTheater);
//		SceneBook sb = myTheater.getSceneBook();
//		CogcharEventActionBinder trigBinder = SceneActions.getBinder();
//		//myWebMapper.connectLiftSceneInterface(myBundleCtx); // Now done in PumaAppContext.initCinema
//		FancyTriggerFacade.registerTriggersForAllScenes(trigBinder, myTheater, sb);
//		myTheater.startThread();
//		doWiringPostStart(optBunCtxForWiring, myTheater);
	}
	protected void doWiringPreStart(BundleContext optBunCtxForWiring, Theater t) {
		
	}
	protected void doWiringPostStart(BundleContext optBunCtxForWiring, Theater t) {
		
	}
	protected void doStopOutputs() {
		
	}
	protected void doResetOutputs() {
		
	}	
	public void stopTheater(boolean cancelOutJobs) {
		// Should be long enough for the 100 Msec loop to cleanly exit.
		// Was 200, but very occasionally this wasn't quite long enough, and myWorkThread was becoming null after the
		// check in Theater.killThread, causing a NPE
		int killTimeWaitMsec = 250; 
		//myWebMapper.disconnectLiftSceneInterface(myBundleCtx); // Now done in PumaAppContext.reloadAll
		myTheater.fullyStop(killTimeWaitMsec, cancelOutJobs);
	}
	
	public Ident getCharIdent() { 
		return myAgentID;
	}
	public void stopEverything() {
		Ident charID = getCharIdent();
		boolean cancelOutJobs = true;
		getLogger().info("stopEverything for {} - Stopping Theater, cancelOutJobs={}", charID, cancelOutJobs);
		stopTheater(cancelOutJobs);
	}

	public void stopAndReset() {
		Ident charID = getCharIdent();
		stopEverything();
		// TODO:  Send character to default positions.
		getLogger().info("stopAndReset for {} - Restarting behavior theater.", charID);
		startTheater(BUNCTX_FOR_CRUDE_RESTART);
		getLogger().info("stopAndReset - Complete.");
	}

	public void stopResetAndRecenter() {
		Ident charID = getCharIdent();
		stopEverything();
		doResetOutputs();
		getLogger().info("stopResetAndRecenter - Restarting behavior theater.");
		startTheater(BUNCTX_FOR_CRUDE_RESTART);
		getLogger().info("stopResetAndRecenter - Complete.");
	}
	public void loadBehaviorConfigFromTestFile(boolean clearCachesFirst) throws Throwable {
//		String pathTail = "bhv_nugget_02.ttl";
//		String behavPath = myBehaviorCE.getBehaviorPermPath(pathTail);
//		// if (useTempFiles) {	//behavPath = behavCE.getBehaviorTempFilePath(pathTail);
//		ClassLoader optCLforJenaFM = org.cogchar.bundle.render.resources.ResourceBundleActivator.class.getClassLoader();
//		TheaterTest.loadSceneBookFromFile(myTheater, behavPath, optCLforJenaFM, clearCachesFirst);
	}
	public void loadBehaviorConfigFromRepo(RepoClient repoClient, Ident chanGraphID, Ident behavGraphID, 
					boolean clearCachesFirst) throws Throwable {
		TheaterTest.loadSceneBookFromRepo(myTheater, repoClient, chanGraphID, behavGraphID, clearCachesFirst);
	}	
	public void usePermAnims() {
		getLogger().warn("usePermAnims() not implemented yet");
	}

	public void useTempAnims() {
		getLogger().warn("useTempAnims() not implemented yet");
	}
}
