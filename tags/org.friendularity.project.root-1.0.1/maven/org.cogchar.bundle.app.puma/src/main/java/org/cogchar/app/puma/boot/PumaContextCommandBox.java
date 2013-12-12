/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import org.appdapter.help.repo.RepoClient;
//import org.cogchar.app.puma.vworld.PumaVirtualWorldMapper;
import org.cogchar.app.puma.config.PumaConfigManager;
import org.cogchar.blob.emit.RenderConfigEmitter;
import org.cogchar.platform.trigger.CogcharScreenBox;
//import org.cogchar.render.app.bony.BonyGameFeatureAdapter;
//import org.cogchar.render.app.bony.BonyRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.model.humanoid.HumanoidFigure;
//import org.cogchar.render.model.humanoid.HumanoidFigureManager;
//import org.cogchar.render.scene.goody.PathMgr;
//import org.cogchar.render.scene.goody.SpatialAnimMgr;
//import org.cogchar.render.sys.goody.GoodyGameFeatureAdapter;


/**
 * This class is intended to be the "public" API to the PUMA "system",
 * for use from direct commands sent by GUIs or network.  
 * 
 * An instance of this object is referred to in our repositories, as 
 * registered with the URI returned by an application's  
 * PumaContextMediator.getSysContextURI() method.
 * 
 * Having "public" methods on this object helps us to keep more
 * of PumaAppContext's methods *protected*, and also keep track
 * in one place of what we're officially *exposing* to the command 
 * layer.
 * 
 * @author Stu B. <www.texpedient.com>
 */

public class PumaContextCommandBox extends CogcharScreenBox {
	private		PumaAppContext		myPAC;
	
	private		ExecutorService		myExecService;
	
	protected PumaContextCommandBox(PumaAppContext pac) {
		myPAC = pac;
	}
//	protected HumanoidRenderContext getHRC() { 
//		return myPAC.getOrMakeVWorldMapper().getHumanoidRenderContext();
//	}
//	public GoodyGameFeatureAdapter getGameFeatureAdapter() { 
//		return getHRC().getGameFeatureAdapter();
//	}
//	public HumanoidFigureManager getFigureManager() { 
//		return getHRC().getHumanoidFigureManager();
//	}
//	public PathMgr getPathMgr() {
//		return getHRC().getGoodyRenderRegistryClient().getScenePathFacade(null);
//	}
//	public SpatialAnimMgr getThingAnimMgr() {
//		return getHRC().getGoodyRenderRegistryClient().getSceneAnimFacade(null);
//	}
//	public void resetMainCameraLocation() { 
//		getHRC().setDefaultCameraLocation();
//	}
//
//	public HumanoidFigure getSinbad() { 
//		BonyRenderContext brc = getHRC();
//		RenderConfigEmitter bce = brc.getConfigEmitter();
//		HumanoidFigureManager hfm = getFigureManager();
//		return hfm.getHumanoidFigure(bce.SINBAD_CHAR_IDENT());
//	}	
//	public PumaVirtualWorldMapper getVWM() { 
//		return myPAC.getOrMakeVWorldMapper();
//	}
	
	private ExecutorService getExecService() { 
		if (myExecService == null) {
			myExecService = Executors.newSingleThreadExecutor();
		}
		return myExecService;
	}
	
	
	final public static String WORLD_CONFIG = "worldconfig";
	final public static String BONE_ROBOT_CONFIG = "bonerobotconfig";
	final public static String MANAGED_GCS = "managedglobalconfigservice";
	final public static String ALL_HUMANOID_CONFIG = "allhumanoidconfig";
	final public static String THING_ACTIONS = "thingactions";
	// Currently used from two places:
	// org/cogchar/app/puma/cgchr/PumaVirtualWorldMapper.java:[74,15] 
	// org/cogchar/app/puma/cgchr/CommandTargetForUseFromWeb.java:[66,25] 
			// which is set up by PumaWebMapper
	public Future<Boolean> processUpdateRequestAsync(final String request, final boolean resetMainConfigFlag) {
		// Do the actual updates on a new thread. That way we don't block the render thread. Much less intrusive, plus this way things
		// we need to enqueue on main render thread will actually complete -  it must not be blocked during some of the update operations!
		// This brings up an interesting point: we are probably doing far too much on the main jME thread!
		logInfo("Requesting async-processing of kind: " + request);
		// boolean success = true;
		ExecutorService execSvc = getExecService();

		Callable<Boolean> c = new Callable<Boolean> () { 
			@Override public Boolean call() {
				return processUpdateRequestNow(request, resetMainConfigFlag);
			}
		};
		Future<Boolean> resultFuture = execSvc.submit(c);
		return resultFuture;
				
	}

	/**
	 * Called only indirectly after scheduling by processUpdateRequestAsync() above.
	 * @param request
	 * @param resetMainConfigFlag
	 * @return 
	 */
	private boolean processUpdateRequestNow(String request, final boolean resetMainConfigFlag) {
		boolean successFlag = true;
		if (WORLD_CONFIG.equals(request.toLowerCase())) {
			//myPAC.initCinema(true);
		} else if (BONE_ROBOT_CONFIG.equals(request.toLowerCase())) {
			myPAC.reloadBoneRobotConfig();
		} else if (MANAGED_GCS.equals(request.toLowerCase())) {
			final PumaConfigManager pcm = myPAC.getConfigManager();
			pcm.clearOSGiComps();
			myPAC.reloadGlobalConfig();
		} else if (ALL_HUMANOID_CONFIG.equals(request.toLowerCase())) {
			// This also calls initCinema
			myPAC.reloadAll(resetMainConfigFlag);
		} else if (THING_ACTIONS.equals(request.toLowerCase())) {
			myPAC.resetMainConfigAndCheckThingActions();
		} else {
			getLogger().warn("PumaAppContext did not recognize the config update to be performed: {}", request);
			successFlag = false;
		}
		return successFlag;
	}
	public 	RepoClient getMainConfigRepoClient() {
		PumaConfigManager pcm = myPAC.getConfigManager();
		return pcm.getMainConfigRepoClient();
	}
}
