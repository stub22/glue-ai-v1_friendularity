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
package org.cogchar.bundle.app.puma;
import java.util.List;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.cogchar.app.puma.config.PumaConfigManager;
import org.cogchar.app.puma.config.PumaGlobalModeManager;
import org.cogchar.app.puma.registry.PumaRegistryClient;
import org.cogchar.app.puma.registry.PumaRegistryClientFinder;
import org.cogchar.app.puma.web.PumaWebMapper;
import org.cogchar.blob.emit.GlobalConfigEmitter;
import org.appdapter.help.repo.RepoClient;
//import org.cogchar.app.puma.vworld.PumaVirtualWorldMapper;
import org.cogchar.impl.thing.basic.BasicThingActionRouter;
import org.osgi.framework.BundleContext;
import org.mechio.api.motion.Robot;
import org.cogchar.bind.rk.robot.motion.CogcharMotionSource;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.impl.channel.AnimFileSpecReader;
import org.cogchar.impl.channel.FancyFile;
import org.cogchar.name.entity.EntityRoleCN;
import static org.cogchar.name.entity.EntityRoleCN.RKRT_NS_PREFIX;
//import org.cogchar.render.sys.module.RenderModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * High level workarounds - vintage Spring+Summer 2013.
 */
public class PumaAppUtils extends BasicDebugger {
	static Logger theLogger = LoggerFactory.getLogger(PumaAppUtils.class);

	/**
	 * This is a crude handle-grabbing entry pont, which assumes "the application"
	 * has already been initialized.  
	 * It is used from:
	 *		PumaAppUtils.attachVWorldRenderModule
	 *		PumaAppUtils.getKnownAnimationFiles
	 * 
	 *		GruesomeTAProcessingFuncs.registerActionConsumers
	 *		GreedyHandleSet.processPendingThingActions
	 * 
	 *		CCRK_DemoActivator.startDeicticMonitoring - New capstone features for viz of gaze, pointing, throwing, kicking
	 *		CCRK_DemoActivator.setupDebuggingScaffold - disabled exposure to Swing GUI
	 *		
	 * 
	 */
	public static class GreedyHandleSet {
		public PumaRegistryClientFinder prcFinder = new PumaRegistryClientFinder();
		public PumaRegistryClient pumaRegClient = prcFinder.getPumaRegClientOrNull(null, PumaRegistryClient.class);
		public final PumaConfigManager pcm = pumaRegClient.getConfigMgr(null);
		public RepoClient rc = pcm.getMainConfigRepoClient();
		public PumaWebMapper pumaWebMapper = pumaRegClient.getWebMapper(null);
		private Ident animPathGraphID = rc.makeIdentForQName(AnimFileSpecReader.animGraphQN());
		
		// Junky old stuff of dubious value.  All three of these classes were created as placeholders for
		// difficult concepts, better treated as free variables than known designs.
		public final PumaGlobalModeManager pgmm = pcm.getGlobalModeMgr();		
		public GlobalConfigEmitter gce = pgmm.getGlobalConfig();
		public BehaviorConfigEmitter animBCE = new BehaviorConfigEmitter(rc, animPathGraphID);
	}

	
	public static List<FancyFile> getKnownAnimationFiles() { 
		GreedyHandleSet srec = new GreedyHandleSet();
		return AnimFileSpecReader.findAnimFileSpecsForJava(srec.animBCE);
	}
	public static 	void startSillyMotionComputersDemoForVWorldOnly(BundleContext bundleCtx, Robot.Id optRobotID_elseAllRobots) { 
		List<CogcharMotionSource> cogMotSrcList = CogcharMotionSource.findCogcharMotionSources(bundleCtx, optRobotID_elseAllRobots);
		for (CogcharMotionSource cms : cogMotSrcList) {
			Robot srcBot = cms.getRobot();
			Robot.Id srcBotID = srcBot.getRobotId();
			theLogger.info("Starting silly motion computer for actual Robot-ID {} found when looking for {} ", srcBotID, optRobotID_elseAllRobots);
			SillyDemoMotionComputer dmc = new SillyDemoMotionComputer();
			cms.addJointComputer(dmc);
		}
	}
//	public static 	void attachVWorldRenderModule(BundleContext bundleCtx, RenderModule rMod, Ident optVWorldSpecID) {
//		GreedyHandleSet srec = new GreedyHandleSet();	
//		PumaVirtualWorldMapper pvwm = srec.pumaRegClient.getVWorldMapper(optVWorldSpecID);
//		if (pvwm != null) {
//			pvwm.attachRenderModule(rMod);
//		} else {
//			theLogger.error("Cannot find VWorld to attach renderModel [optVWorldSpecID={}]", optVWorldSpecID);
//		}
//	} 
}
