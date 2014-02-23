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
package org.friendularity.bundle.demo.ccmio;

import org.appdapter.core.name.Ident;

import org.cogchar.render.sys.module.RenderModule;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.bundle.app.puma.PumaAppUtils.GreedyHandleSet;
import org.cogchar.bundle.app.vworld.startup.PumaVirtualWorldMapper;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class CCMIO_VWorldHelper {
	static Logger theLogger = LoggerFactory.getLogger(CCMIO_VWorldHelper.class);
	// 2014-02-20      PumaAppUtils.attachVWorldRenderModule(bundleCtx, werm, null);
	public  	void attachVWorldRenderModule(BundleContext bundleCtx, RenderModule rMod, Ident optVWorldSpecID) {
		// srec-access not currently used *directly*, but we will probly want it again.
		GreedyHandleSet srec = PumaAppUtils.obtainGreedyHandleSet();
		// Old way:
		// PumaVirtualWorldMapper pvwm = srec.pumaRegClient.getVWorldMapper(optVWorldSpecID);
		PumaVirtualWorldMapper pvwm = getVWorldMapper(optVWorldSpecID);
		if (pvwm != null) {
			pvwm.attachRenderModule(rMod);
		} else {
			theLogger.error("Cannot find VWorld to attach renderModel [optVWorldSpecID={}]", optVWorldSpecID);
		}
	} 
	private PumaVirtualWorldMapper	myVWorldMapper;
	public PumaVirtualWorldMapper getVWorldMapper(Ident optSpecID) {
		return myVWorldMapper;
	}
	// Who is gonna call this now?
	
	public void putVWorldMapper(PumaVirtualWorldMapper vwm, Ident optSpecID) {
		myVWorldMapper = vwm;
	}	
	
	//  2014-02-22 - Noted most of the stuff below is now captured in the new VWorldRegistry class.
	
	/*  2014-02-20 removed from PumaAppContext
	 * 	public boolean hasVWorldMapper() {
		return (myRegClient.getVWorldMapper(null) != null);
	}

	public PumaVirtualWorldMapper getOrMakeVWorldMapper() {
		PumaVirtualWorldMapper pvwm = myRegClient.getVWorldMapper(null);
		if (pvwm == null) {
			pvwm = new PumaVirtualWorldMapper(this);
			myRegClient.putVWorldMapper(pvwm, null);
		}
		return pvwm;
	}
	protected void startOpenGLCanvas(boolean wrapInJFrameFlag, java.awt.event.WindowListener optWinLis) throws Exception {
		if (hasVWorldMapper()) {
			PumaVirtualWorldMapper pvwm = myRegClient.getVWorldMapper(null);
			pvwm.startOpenGLCanvas(wrapInJFrameFlag, optWinLis);
		} else {
			getLogger().warn("Ignoring startOpenGLCanvas command - no vWorldMapper present");
		}
	}
protected void initCinema(boolean clearFirst) {
		if (hasVWorldMapper()) {
			PumaVirtualWorldMapper pvwm = myRegClient.getVWorldMapper(null);
			if (clearFirst) {
				pvwm.clearCinematicStuff();
			}
			CommandSpace cmdSpc = myRegClient.getCommandSpace(null);
			PumaConfigManager pcm = getConfigManager();
			BasicThingActionRouter router = GruesomeTAProcessingFuncs.getActionRouter();
			pvwm.initVirtualWorlds(cmdSpc, pcm, router);
//          Moved connectWeb call to PumaBooter so we can get lifter without the VWorld - Matt, Sep 20 2013
//			connectWeb();

			ClassLoader vizResCL = getSingleClassLoaderOrNull(ResourceFileCategory.RESFILE_OPENGL_JME3_OGRE);
			pvwm.connectVisualizationResources(vizResCL);
		} else {
			getLogger().warn("Ignoring initCinema command - no vWorldMapper present");
		}
	}
	* // protected void stopAndReleaseAllHumanoids() {
	//...
			PumaVirtualWorldMapper pvwm = getOrMakeVWorldMapper();
		pvwm.detachAllHumanoidFigures();
		* }
	//...
	protected void disconnectAllCharsAndMappers() throws Throwable {
		BundleContext bunCtx = getBundleContext();

		if (hasVWorldMapper()) {
			PumaVirtualWorldMapper vWorldMapper = getOrMakeVWorldMapper();
			vWorldMapper.clearCinematicStuff();
			// Consider:  also set the context/registry vWorldMapper to null, expecting
			// PumaBooter or somesuch to find it again.
		}
	//...
	protected void reloadAll(boolean resetMainConfigFlag) {
	  initCinema(true);
	 */
	
// 2014-02-20  these were removed from PumaRegistryClient and Impl
//	public PumaVirtualWorldMapper getVWorldMapper(Ident optSpecID);
//	public void putVWorldMapper (PumaVirtualWorldMapper v, Ident optSpecID);	
// Impl: 
	//private PumaVirtualWorldMapper	myVWorldMapper;
	//	@Override public PumaVirtualWorldMapper getVWorldMapper(Ident optSpecID) {
//		return myVWorldMapper;
//	}
//
//	@Override public void putVWorldMapper(PumaVirtualWorldMapper vwm, Ident optSpecID) {
//		myVWorldMapper = vwm;
//	}
	
	
// From PumaBooter:
//         boolean wantVWorld = mediator.getFlagIncludeVirtualWorld();
//		if (wantVWorld) {
//			initVWorldUnsafe(pac, mediator);
//		}	
	
//        if (wantVWorld) {
//            getLogger().debug("%%% connectDualRobotChars() completed , calling initCinema()");
//
//            // Lights, Cameras, and Cinematics were once configured during PumaDualCharacter init
//            // Since we can support multiple characters now (and connect cameras to them), this needs to 
//            // happen after connectDualRobotChars().
//            // We'll let pac take care of this, since it is currently "Home of the Global Mode"
//            //pac.initCinema(false);
//        }
//	private void initVWorldUnsafe(final PumaAppContext pac, PumaContextMediator mediator) throws Throwable {
//		// Mediator must be able to decide panelKind before the HumanoidRenderContext is built.
//		String panelKind = mediator.getPanelKind();
//		getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling initHumanoidRenderContext()");
//		PumaVirtualWorldMapper pvwm = pac.getOrMakeVWorldMapper();
//		HumanoidRenderContext hrc = pvwm.initHumanoidRenderContext(panelKind);
//		getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling mediator.notifyPanelsConstructed()");
//		mediator.notifyPanelsConstructed(pac);
//		/*
//		 * Start up the JME OpenGL canvas, which will in turn initialize the Cogchar rendering "App" (in JME3 lingo).
//		 *
//		 * Firing up the OpenGL canvas requires access to sun.misc.Unsafe, which must be explicitly imported by
//		 * ext.bundle.osgi.jmonkey, and explicitly allowed by the container when using Netigso
//		 */
//		boolean allowJFrames = mediator.getFlagAllowJFrames();
//		if (allowJFrames) {
//			WindowAdapter winLis = new WindowAdapter() {
//				@Override public void	windowClosed(WindowEvent e) {
//					getLogger().warn("PumaBooter caught window CLOSED event for OpenGL frame:  {}", e);
//					notifyVWorldWindowClosed();
//				}
//			};
//			getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling startOpenGLCanvas");
//			pac.startOpenGLCanvas(allowJFrames, winLis);
//			
//		}
//		getLogger().debug("%%%%%%%%%%%%%%%%%%% startOpenGLCanvas completed, enqueueing final boot phase on JME3 thread, and waiting for completion.");
//
//		/**
//		 * Populate the virtual world with humanoids, cameras, lights, and other goodies. This step will load all the 3D
//		 * models (and other rendering resources) that Cogchar needs, based on what is implied by the sysContextURI we
//		 * supplied to the PumaAppContext constructor above.
//		 *
//		 * We enqueue this work to occur,on JME3 update thread. Otherwise we'll get an: IllegalStateException: Scene
//		 * graph is not properly updated for rendering.
//		 * 
//		 * This call first waits for a WorkaroundAppStub to appear, so that it can enqueue the main call on the JME3 
//		 * thread.  THEN it waits for that enqueued call to finish, allowing us to be sure that all this init is
//		 * completed, on the proper thread, before we continue the PUMA boot process.
//		 */					
//		hrc.runPostInitLaunchOnJmeThread();
//		getLogger().debug("%%%%%%%%%%%%%%%%%%% Context.runPostInitLaunch completed");
//	}
	/*-------------------------------------------------
	 * From PumaContextCommandBox:
	 
	 * 	protected HumanoidRenderContext getHRC() { 
		return myPAC.getOrMakeVWorldMapper().getHumanoidRenderContext();
	}
	public GoodyGameFeatureAdapter getGameFeatureAdapter() { 
		return getHRC().getGameFeatureAdapter();
	}
	public HumanoidFigureManager getFigureManager() { 
		return getHRC().getHumanoidFigureManager();
	}
	public PathMgr getPathMgr() {
		return getHRC().getGoodyRenderRegistryClient().getScenePathFacade(null);
	}
	public SpatialAnimMgr getThingAnimMgr() {
		return getHRC().getGoodyRenderRegistryClient().getSceneAnimFacade(null);
	}
	public void resetMainCameraLocation() { 
		getHRC().setDefaultCameraLocation();
	}

	public HumanoidFigure_SinbadTest getSinbad() { 
		BonyRenderContext brc = getHRC();
		RenderConfigEmitter bce = brc.getConfigEmitter();
		HumanoidFigureManager hfm = getFigureManager();
		return (HumanoidFigure_SinbadTest) hfm.getHumanoidFigure(bce.SINBAD_CHAR_IDENT());
	}	
	public PumaVirtualWorldMapper getVWM() { 
		return myPAC.getOrMakeVWorldMapper();
	}
	* 
	private boolean processUpdateRequestNow(String request, final boolean resetMainConfigFlag) {
		boolean successFlag = true;
		if (WORLD_CONFIG.equals(request.toLowerCase())) {
			//myPAC.initCinema(true);* 
	
	 */
}
