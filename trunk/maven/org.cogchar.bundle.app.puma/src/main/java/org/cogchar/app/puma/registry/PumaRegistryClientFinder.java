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

package org.cogchar.app.puma.registry;
import org.appdapter.api.facade.FacadeSpec;
import org.appdapter.subreg.SubsystemHandle;
import org.appdapter.subreg.BasicSubsystemHandle;
import org.appdapter.subreg.FacadeHandle;
import org.cogchar.blob.emit.SubsystemHandleFinder;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class PumaRegistryClientFinder {
	protected enum PFKind {
		PUMA_REG_CLIENT
	}
	protected static class PFSpec<PFType> extends FacadeSpec<PFType, PFKind> {
		PFSpec(PFKind kind, Class<PFType> sClz, boolean extFlag) {
			super(kind, sClz, extFlag);
		}
	}	
	protected static PFSpec<PumaRegistryClient> 		THE_PUMA_REGISTRY_CLIENT_Spec;
	static {
		// true = "external" - explicit registration pattern
		THE_PUMA_REGISTRY_CLIENT_Spec = new PFSpec<PumaRegistryClient>(PFKind.PUMA_REG_CLIENT, PumaRegistryClient.class, true);
	}
	public void registerPumaRegClient(PumaRegistryClient prcFacade, String optOverrideName, Class optCredClaz) {
		PFSpec<PumaRegistryClient> regClientFSpec = THE_PUMA_REGISTRY_CLIENT_Spec;
		SubsystemHandle shand = SubsystemHandleFinder.getSubsystemHandle(SubsystemHandleFinder.SUBSYS_REG_PUMA(), regClientFSpec,  optCredClaz);		
		shand.registerExternalFacade(regClientFSpec, prcFacade, optOverrideName);			
	}
	public PumaRegistryClient getPumaRegClientOrNull(String optOverrideName, Class optCredClaz) {
		PumaRegistryClient pumaRegClient = null;
		PFSpec<PumaRegistryClient> regClientFSpec = THE_PUMA_REGISTRY_CLIENT_Spec;
		SubsystemHandle shand = SubsystemHandleFinder.getSubsystemHandle(SubsystemHandleFinder.SUBSYS_REG_PUMA(), regClientFSpec,  optCredClaz);
		FacadeHandle<PumaRegistryClient> prcHandle = shand.findExternalFacade(regClientFSpec, optOverrideName);
		if (prcHandle.isReady()) {
			pumaRegClient = prcHandle.getOrElse(null);
		} 
		return pumaRegClient;
	}

}
