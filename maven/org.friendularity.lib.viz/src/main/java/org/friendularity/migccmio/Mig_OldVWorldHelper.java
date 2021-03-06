/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.cogchar.api.thing.WantsThingAction;
import org.cogchar.bind.mio.robot.motion.CogcharMotionSource;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.bind.symja.MathSpaceFactory;
/*
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.bundle.app.puma.PumaAppUtils.GreedyHandleSet;
import org.cogchar.bundle.app.vworld.central.VWorldRegistry;
import org.cogchar.bundle.app.vworld.central.VirtualWorldFactory;
import org.cogchar.bundle.app.vworld.startup.PumaVirtualWorldMapper;
import org.cogchar.impl.thing.route.BasicThingActionRouter;
*/
import org.cogchar.render.sys.module.RenderModule;
import org.friendularity.api.west.WorldEstimate;
/*
import org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator;
import org.friendularity.bundle.demo.ccmio.CCMIO_DemoMotionComputer;
import org.friendularity.bundle.demo.ccmio.CCMIO_WorldEstimateRenderModule;
*/
import org.friendularity.impl.visual.EstimateVisualizer;
import org.friendularity.migccmio.Mig_DemoMidiCommandMapper.MidiLaunchWrapper;
import org.mechio.api.motion.Robot;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Properties;

// import org.friendularity.bundle.demo.ccmio.CCMIO_DemoMidiCommandMapper.MidiLaunchWrapper;

/**
 * @author Stu B22 <stub22@appstract.com>
 *
 * From 2014 era, performs final setup + content-population tasks for CCMIO demo after the
 * VWorld is launched.
 *
 * This class is instantiated via a lifecycle that depends on the VWorldRegistry being available.
 */
public class Mig_OldVWorldHelper extends BasicDebugger {

	static Logger theLogger = LoggerFactory.getLogger(Mig_OldVWorldHelper.class);

	private Mig_WorldEstimateRenderModule myWERM;
	private Mig_DemoMidiCommandMapper 		myMidiMapper;
	// private PumaVirtualWorldMapper 				myVWorldMapper;
	private WantsThingAction					myTARouter;


	public void doWermStuff() {
		getLogger().info("doWermStuff - BEGIN");
		// Hey, let's get some fused-sensor-data visualization going too, while we're at it!

		myWERM = null; // new Mig_WorldEstimateRenderModule();

		getLogger().info("doWermStuff - starting initMapperWithFeatures");
		MidiLaunchWrapper mlw = new MidiLaunchWrapper();

		myMidiMapper = mlw.initMapperWithFeatures();
		getLogger().info("doWermStuff - finished initMapperWithFeatures()");
		if (myMidiMapper != null) {
			// TODO:  Can probably remove this link
			myMidiMapper.setWERM(myWERM);
			getLogger().info("doWermStuff - setting JVisionTexture flag");
			myWERM.setMidiMapper(myMidiMapper);
		}
		// Enable/Disable this texture flow based on whether we are launching JVision or not.
		// Should be a dont-care whether this happens before/after   startVisionMonitors() below.
		// TODO:  Re-verify in detail.
		getLogger().info("doWermStuff - setting JVisionTexture flag");


		// myWERM.setFlag_JVisionTextureRoutingEnabled(CCMIO_DemoActivator.myFlag_connectJVision);

		getLogger().info("$$$$$$$$$$$$$$$    $$$$$$$$$$$$$$$$$   disabled PumaAppUtils.attachVWorldRenderModule");
		getLogger().info("doWermStuff - END");
	}
/*
	public void attachVWorldRenderModule(// BundleContext bundleCtx, 
			RenderModule rMod, Ident optVWorldSpecID) {
		// srec-access not currently used *directly*, but we will probly want it again.
		GreedyHandleSet srec = PumaAppUtils.obtainGreedyHandleSet();
		PumaVirtualWorldMapper pvwm = getVWorldMapper(optVWorldSpecID);
		if (pvwm != null) {
			theLogger.info("Attaching RenderModule {} to VWorldMapper {}", rMod, pvwm);
			pvwm.attachRenderModule(rMod);
		} else {
			theLogger.error("Cannot find VWorld to attach renderModel [optVWorldSpecID={}]", optVWorldSpecID);
		}
	}

	// Now that we see this works with new-PUMA+Vworld setup, we can consider switching over to just using the 
	// VWorldRegistry (not the Mapper).
	private PumaVirtualWorldMapper getVWorldMapper(Ident optSpecID) {
		if (myVWorldMapper == null) {
			theLogger.warn("Obsolete call to lookupVWorldMapperDirectly");
			myVWorldMapper = lookupVWorldMapperDirectly(optSpecID);
		}
		return myVWorldMapper;
	}

	private void setVWorldMapper(PumaVirtualWorldMapper vwm, Ident optSpecID) {
		myVWorldMapper = vwm;
	}

	private void setTARouter(WantsThingAction taRouter) {
		myTARouter = taRouter;
	}

	public void completeSetup(VWorldRegistry vwReg) {
		WantsThingAction taRouter = vwReg.getRouter();

		Map<Ident, List<WantsThingAction>> routerInternalMap
				= ((BasicThingActionRouter) taRouter).hackExposeConsumerMap();
		getLogger().info("routerInternalMap={}", routerInternalMap);
		for (Ident k : routerInternalMap.keySet()) {
			getLogger().info("graphID={}", k);
			List<WantsThingAction> wanters = routerInternalMap.get(k);
			for (WantsThingAction wta : wanters) {
				getLogger().info("wanter={}", wta);
			}
		}

		setTARouter(taRouter);
		PumaVirtualWorldMapper pvwm = vwReg.getVWM();
		// Tell the helper about the VWorld.
		setVWorldMapper(pvwm, null);
		doWermDemoSetup();
	}
	public void doWermDemoSetup() {
		// Do some loosely defined VWorld visualization setup.
		doWermStuff();
		Bundle b = FrameworkUtil.getBundle(OldVWorldHelper.class);
		if (b != null) {
			BundleContext bundleCtx = b.getBundleContext();
			//
			finishDemoSetup(bundleCtx);
		}

	}
	*/
	private void finishDemoSetup(BundleContext bundleCtx) {
		// Under what conditions should this call succeed?
	//	attachVWorldRenderModule(myWERM, null);

		EstimateVisualizer eViz = myWERM.setupVisualizer(null, null, null);
		// Needs to be done at least once for the selfEstim to exist.
		MathSpaceFactory msf = new MathSpaceFactory();
		// Decided we don't want extra layer of "Scripted"; in part b/c old impl wanted to cache evaluation results.
		// MathGate mg = msf.makeScriptedMathGate();
		MathGate mg = msf.makeUnscriptedMathGate();
		myWERM.setMathGate(mg);
		Ident worldEstimID = new FreeIdent(WorldEstimate.ESTIM_NS + "world_estim_31");
		WorldEstimate we = new WorldEstimate(worldEstimID);
		myWERM.setWorldEstimate(we);
		Robot.Id optRobotID_elseAllRobots = new Robot.Id("Sinbad");
		startMotionComputers(bundleCtx, optRobotID_elseAllRobots, we);

		// Force some animation to play

	}

	private void startMotionComputers(BundleContext bundleCtx, Robot.Id optRobotID_elseAllRobots, WorldEstimate we) {
		List<CogcharMotionSource> cogMotSrcList = CogcharMotionSource.findCogcharMotionSources(bundleCtx, optRobotID_elseAllRobots);

		for (CogcharMotionSource cms : cogMotSrcList) {
			Robot srcBot = cms.getRobot();
			Robot.Id srcBotID = srcBot.getRobotId();
			if ((optRobotID_elseAllRobots == null) || optRobotID_elseAllRobots.equals(srcBotID)) {
				getLogger().info("Found CogcharMotionSource for Robot-ID {} matching pattern {}", srcBotID, optRobotID_elseAllRobots);
				// Start a motion computer implemented locally in demo CCRK - tries to move each of Sinbads joints in turn
				Mig_DemoMotionComputer dmc = new Mig_DemoMotionComputer();
				dmc.setWorldEstimate(we);
				cms.addJointComputer(dmc);
				// append a trivial Sinbad-waist-sinusoid-demo implemented in CC-Puma.  Because it acts last, it has last
				// word, but should not unnecessarily override joint-pos from "earlier" phases=computers.


			//	PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(bundleCtx, srcBotID);

			} else {
				getLogger().info("Skipping Robot-ID {} because it doesn't match pattern {}", srcBotID, optRobotID_elseAllRobots);
			}
		}
	}

	/*
	private PumaVirtualWorldMapper lookupVWorldMapperDirectly(Ident optSpecID) {
		// PumaVirtualWorldMapper pvwm = null;

		BundleContext bc = VirtualWorldFactory.getBundleContext();
		Properties props = new Properties();

		VWorldRegistry vwr = VirtualWorldFactory.getOSGiVWorldMapper(bc, props);
		theLogger.info("VWorldRegistry = {}", vwr);
		if (vwr != null) {
			myVWorldMapper = vwr.getVWM();
			theLogger.info("VWorldMapper = {}", myVWorldMapper);
		} else {
			theLogger.warn("Cannot resolve VWorldRegistry");
		}
		return myVWorldMapper;
	}
	*/
}
