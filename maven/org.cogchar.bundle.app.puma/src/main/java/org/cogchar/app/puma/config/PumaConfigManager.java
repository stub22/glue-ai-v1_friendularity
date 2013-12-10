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
package org.cogchar.app.puma.config;

import java.util.List;
import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.appdapter.impl.store.FancyRepo;

import org.cogchar.app.buddy.busker.TriggerItems;
import org.cogchar.blob.emit.GlobalConfigEmitter;



import org.osgi.framework.BundleContext;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;

import org.cogchar.platform.trigger.BoxSpace;
import org.cogchar.platform.trigger.CommandSpace;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class PumaConfigManager {
	// A query interface instance we can reuse - right now just to trigger repo reloads. May want to do that via
	// GlobalConfigEmitter or some other interface in the long run...?

	private RepoClient myCurrentMainConfigRepoClient;

	// Same with the managed queryinterface used by Lift
	OSGiComponent myQueryComp;
	
	private		PumaGlobalModeManager	myGlobalModeMgr = new PumaGlobalModeManager();


	abstract public void applyDefaultRepoClientAsMainConfig(PumaContextMediator mediator, BundleContext optBundCtxForLifecycle);
	
	public PumaGlobalModeManager getGlobalModeMgr() {
		return myGlobalModeMgr;
	}
	protected void setMainConfigRepoClient(RepoClient rc) {
		myCurrentMainConfigRepoClient = rc;
	}
	public void clearMainConfigRepoClient() {
		myCurrentMainConfigRepoClient = null;
	}
	public RepoClient getMainConfigRepoClient() {
		return myCurrentMainConfigRepoClient;
	}
	
	public RepoClient getOrMakeMainConfigRepoClient(PumaContextMediator mediator, BundleContext optBundCtxForLifecycle) {
		if (myCurrentMainConfigRepoClient == null) {
			applyDefaultRepoClientAsMainConfig(mediator, optBundCtxForLifecycle);
			// applyFreshDefaultMainRepoClientToGlobalConfig(optBundCtxForLifecycle);
		}
		return myCurrentMainConfigRepoClient;
	}
 

	/*
	// Used to be called "updateGlobalConfig" - Currently this would cause a detach from any previous lifecycle-registered RepoCli.
	public void applyFreshDefaultMainRepoClientToGlobalConfig(BundleContext optBundCtxForLifecycle) {
		clearMainConfigRepoClient();
		applyGlobalConfig(optBundCtxForLifecycle);
	}
	* 
	*/ 

	
	// TODO : This can be pushed down into o.c.lib.core
	protected static OSGiComponent startRepoClientLifecyle(BundleContext bundCtx, RepoClient rc) {
		OSGiComponent rcComp = null;
		if (rc != null) {
			ServiceLifecycleProvider lifecycle = new SimpleLifecycle(rc, RepoClient.class);
			rcComp = new OSGiComponent(bundCtx, lifecycle);
			rcComp.start();
		}
		return rcComp;
	}
	
	public void clearOSGiComps() {
		myGlobalModeMgr.clearOSGiComps();
		if (myQueryComp != null) {
			myQueryComp.dispose();
		}
	}
	
	public void populateCommandSpace(CommandSpace cmdSpc, BoxSpace boxSpc) {
		RepoClient repoCli = getMainConfigRepoClient();
		TriggerItems.populateCommandSpace(repoCli, cmdSpc, boxSpc);
	}
}
