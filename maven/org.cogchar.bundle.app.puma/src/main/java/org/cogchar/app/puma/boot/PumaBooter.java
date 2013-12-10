/*  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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
package org.cogchar.app.puma.boot;

import org.cogchar.app.puma.config.PumaContextMediator;
//import org.cogchar.app.puma.vworld.PumaVirtualWorldMapper;
import java.util.ArrayList;
import java.util.List;
import org.appdapter.core.log.BasicDebugger;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.appdapter.core.store.Repo;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import org.cogchar.bundle.app.puma.GruesomeTAProcessingFuncs;
import org.cogchar.bundle.app.puma.PumaAppUtils;


import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class PumaBooter extends BasicDebugger {

	public enum BootStatus {

		BOOTING,
		BOOTED_OK,
		BOOT_FAILED
	}

	public class BootResult {

		public BootStatus myStatus;
		public List<String> myMessageList = new ArrayList<String>();
		public Throwable myThrowable;
	}
	/**
	 * Public entry point for the PUMA application system when running in an OSGi environment. Presumes that an SLF4J logging
	 * binding is already in place. Normally this is *not* called directly from another bundle's activator. Instead, it
	 * is called after all initial bundles (for your application) are loaded, which is signaled in a FrameworkEvent
	 *
	 * @param bundleCtx - an OSGi bundle
	 * @param mediator - users customize this object to configure/influence the boot process.
	 * @return a BootResult indicating the status of boot attempt. No app data is passed here.
	 */
	public BootResult bootUnderOSGi(BundleContext bundleCtx, PumaContextMediator mediator) {
		logInfo("%%%%%%%%%%%%%%%%%%% Beginning bootUnderOSGi");
		logError("^^^^ not really an error - just high priority ^^^^^^^^ SLF4J ILoggerFactory = " + LoggerFactory.getILoggerFactory());
		BootResult result = new BootResult();
		result.myStatus = BootStatus.BOOTING;
		try {

			pumaBootUnsafeUnderOSGi(bundleCtx, mediator);

			getLogger().info("%%%%%%%%%%%%%%%%%%%%%%%%% pumaBootUnsafe() completed without exception - PUMA BOOT SUCCESSFUL!  8-)");
			result.myStatus = BootStatus.BOOTED_OK;
		} catch (Throwable t) {
			getLogger().error("Error in PumaBooter 8-(", t);
			result.myStatus = BootStatus.BOOT_FAILED;
			result.myThrowable = t;
		}
		return result;
	}
	/**
	 * Protected initialization of an OSGi context - the guts.
	 * Normally this is called from bootUnderOSGi, which is called from a 
	 * FrameworkStarted-EventHandler (NOT the BundleActivator.start() method !) 
	 * of some "top" application bundle client, e.g. Friendularity CCRK_DemoActivator.
	 * That client controls our boot process through supplied mediator.
	 *
	 * @param bundleCtx
	 * @param mediator
	 * @throws Throwable
	 */
	protected void pumaBootUnsafeUnderOSGi(BundleContext bundleCtx, PumaContextMediator mediator) throws Throwable {
		//   forceLog4jConfig()
		//  Stu 2012-10-27 - the fileSys root isn't being used currently.  
		String optFilesysRoot = mediator.getOptionalFilesysRoot();

		String ctxURI = mediator.getSysContextRootURI();
		
		Ident ctxID = new FreeIdent(ctxURI);
		getLogger().warn("%%% Creating PumaAppContext at [{}]", ctxID);
		final PumaAppContext pac = new PumaAppContext(bundleCtx, mediator, ctxID);

		/*
		 * At this point we have blank, generic PAC + HRC (if vWorld) context objects to work with. PAC + HRC are
		 * typically each a singleton, but not assumed to be. No characters or config have been populated, no OpenGL
		 * window has been opened, and no connection has been made to Robokind.
		 */

		getLogger().debug("%%% Starting repository-backed config services");

		pac.startRepositoryConfigServices();

		/*The mediator should now do any special init that it wants to, but without assuming GUI exists.
		 * This stage includes getting the FIRST whack (both read and write) at the configuration services.  
		 * However, the default mediator impl does nothing.  */

		mediator.notifyContextBuilt(pac);
		// Mediator must have an answer for this before any config is loaded, presently.
//		boolean wantVWorld = mediator.getFlagIncludeVirtualWorld();
//		if (wantVWorld) {
//			initVWorldUnsafe(pac, mediator);
//		}
		/*
		 * Connect the Cogchar PUMA application (configured by implications of the sysContextURI and
		 * sysLocalTempConfigDir used in setupConfigEmitters() above). The result is a list of connected "dual"
		 * characters, which each have a presence in both Cogchar virtual space and Robokind physical space.			 *
		 * If we try to do this inside the JME3Thread callable above (under certain conditions), we can get hung up when
		 * RobotServiceContext calls RobotUtils.registerRobot()
		 */

		
		boolean wantChars = mediator.getFlagIncludeCharacters();
		if (wantChars) {

			getLogger().debug("%%% calling connectDualRobotChars()");
			
			pac.connectAllBodies();

			pac.reloadCommandSpace();
			
			mediator.notifyCharactersLoaded(pac);
		//}
        //We should chek the flag, but oglweb doesn't have it on, so we'll go with the chars flag - Matt
        //if(mediator.getFlagIncludeWebServices()){
            pac.connectWeb();
        }
		
		// NOW we are ready to set up the command-processing system, making use of our boxSpace to find
		// characters and other commandable entities.
		
//		if (wantVWorld) {
//			getLogger().debug("%%% connectDualRobotChars() completed , calling initCinema()");
//
//			// Lights, Cameras, and Cinematics were once configured during PumaDualCharacter init
//			// Since we can support multiple characters now (and connect cameras to them), this needs to 
//			// happen after connectDualRobotChars().
//			// We'll let pac take care of this, since it is currently "Home of the Global Mode"
//			pac.initCinema(false);
//		}
		GruesomeTAProcessingFuncs.registerActionConsumers();
		mediator.notifyBeforeBootComplete(pac);
	}



	private Repo findMainRepo(PumaContextMediator mediator) {
		Repo r = null;

		return r;
	}

	private void initVWorldUnsafe(final PumaAppContext pac, PumaContextMediator mediator) throws Throwable {
		// Mediator must be able to decide panelKind before the HumanoidRenderContext is built.
		String panelKind = mediator.getPanelKind();
		getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling initHumanoidRenderContext()");
		//PumaVirtualWorldMapper pvwm = pac.getOrMakeVWorldMapper();
		//HumanoidRenderContext hrc = pvwm.initHumanoidRenderContext(panelKind);
		getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling mediator.notifyPanelsConstructed()");
		mediator.notifyPanelsConstructed(pac);
		/*
		 * Start up the JME OpenGL canvas, which will in turn initialize the Cogchar rendering "App" (in JME3 lingo).
		 *
		 * Firing up the OpenGL canvas requires access to sun.misc.Unsafe, which must be explicitly imported by
		 * ext.bundle.osgi.jmonkey, and explicitly allowed by the container when using Netigso
		 */
		boolean allowJFrames = mediator.getFlagAllowJFrames();
		if (allowJFrames) {
			WindowAdapter winLis = new WindowAdapter() {
				@Override public void	windowClosed(WindowEvent e) {
					getLogger().warn("PumaBooter caught window CLOSED event for OpenGL frame:  {}", e);
					notifyVWorldWindowClosed();
				}
			};
			getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling startOpenGLCanvas");
			//pac.startOpenGLCanvas(allowJFrames, winLis);
			
		}
		getLogger().debug("%%%%%%%%%%%%%%%%%%% startOpenGLCanvas completed, enqueueing final boot phase on JME3 thread, and waiting for completion.");

		/**
		 * Populate the virtual world with humanoids, cameras, lights, and other goodies. This step will load all the 3D
		 * models (and other rendering resources) that Cogchar needs, based on what is implied by the sysContextURI we
		 * supplied to the PumaAppContext constructor above.
		 *
		 * We enqueue this work to occur,on JME3 update thread. Otherwise we'll get an: IllegalStateException: Scene
		 * graph is not properly updated for rendering.
		 * 
		 * This call first waits for a WorkaroundAppStub to appear, so that it can enqueue the main call on the JME3 
		 * thread.  THEN it waits for that enqueued call to finish, allowing us to be sure that all this init is
		 * completed, on the proper thread, before we continue the PUMA boot process.
		 */					
		//hrc.runPostInitLaunchOnJmeThread();
		getLogger().debug("%%%%%%%%%%%%%%%%%%% Context.runPostInitLaunch completed");
	}
	protected void notifyVWorldWindowClosed() { 
		Bundle anyB = org.osgi.framework.FrameworkUtil.getBundle(getClass());
		BundleContext anyBC = anyB.getBundleContext();
		shutdownOSGiContainer(anyBC);
	}
	protected void shutdownOSGiContainer(BundleContext bc) { 
		Bundle sysB = bc.getBundle(0);
		getLogger().warn("PumaBooter asking system bundle to stop(): {}", sysB);
		try {
			sysB.stop();
		} catch (Throwable t) {
			getLogger().error("PumaBooter caught exception during sys-bundle.stop() request", t);
		}
	}
}
