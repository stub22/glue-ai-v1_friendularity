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

import org.appdapter.core.matdat.RepoSpec;
import org.osgi.framework.BundleContext;

import org.appdapter.core.store.Repo;
import org.appdapter.help.repo.RepoClient;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.jflux.impl.services.rk.osgi.OSGiUtils;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * A mix-in encoding an optional assumption that the best "default" config-repo-client 
 * is the one derived from the current RepoSpec provided by our current mediator.   
 * 
 * As of 2012-10-15, this fetching of specs from the mediator is the crux of
 * Cogchar PUMA boot customization.
 */

public class VanillaConfigManager extends PumaConfigManager {
	/** We override this method to plug in to PumaConfigManager's operations,
	 * calling applyDefaultRepoClientAsMainConfig.
	 * 
	 * @param mediator
	 * @param optBundCtxForLifecycle 
	 */
	@Override public void applyDefaultRepoClientAsMainConfig(PumaContextMediator mediator, BundleContext optBundCtxForLifecycle) {
		applyVanillaRepoClientAsMainConfig(mediator, optBundCtxForLifecycle);
	}
	/**
	 * 1) Calls makeVanillaRepoClient.
	 * 2) If OSGi bundle is present, starts lifecycles using it.
	 * 
	 * Does not yet perform any cleanup on old lifecycles.
	 * TODO:  Add any such necessary cleanup.
	 * @param mediator
	 * @param optBundCtxForLifecycle - optional (null => None) OSGi BundleContext signaling that JFlux.org/Robokind.org 
	 * -compatible lifecycles should be started.
	 */
	protected void applyVanillaRepoClientAsMainConfig( PumaContextMediator mediator, BundleContext optBundCtxForLifecycle) {
        RepoClient repoClient = getExistingRepoClient(optBundCtxForLifecycle);
        // TODO:  "turn off" any previous config's lifecycle
        if(repoClient == null){
            repoClient = makeVanillaRepoClient(mediator);
        }
		if (repoClient != null) {
			setMainConfigRepoClient(repoClient);
			if (optBundCtxForLifecycle != null)  {
				myQueryComp = startRepoClientLifecyle(optBundCtxForLifecycle, repoClient);
			}
		}
	}
    
    private RepoClient getExistingRepoClient(BundleContext context){
        ServiceReference[] refs = null;
        try{
            refs = context.getServiceReferences(RepoClient.class.getName(), null);
        }catch(InvalidSyntaxException ex){ }
        if(refs == null){
            return null;
        }
        for(ServiceReference ref : refs){
            RepoClient rc = OSGiUtils.getService(RepoClient.class, context, ref);
            if(rc != null){
                return rc;
            }
        }
        return null;
    }
	/**
	 * Ask client Mediator for its MainConfig RepoSpec, and then make a repo for that spec,
	 * thus implementing the crux of Cogchar-PUMA boot customization.
	 * @param mediator
	 * @return 
	 */
	private static RepoClient makeVanillaRepoClient(PumaContextMediator mediator) {
		RepoSpec rspec = mediator.getMainConfigRepoSpec();
				
		Repo.WithDirectory testRepo = rspec.makeRepo();
		RepoClient repoCli = rspec.makeRepoClient(testRepo);
		return repoCli;
	}
	

}
