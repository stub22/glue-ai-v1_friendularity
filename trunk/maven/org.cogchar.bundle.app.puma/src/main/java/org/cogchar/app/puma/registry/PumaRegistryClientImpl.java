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

import java.util.ArrayList;
import java.util.List;
import org.apache.avro.generic.GenericData;
import org.appdapter.core.name.Ident;
import org.cogchar.app.puma.config.PumaConfigManager;
import org.cogchar.app.puma.config.PumaContextMediator;
//import org.cogchar.app.puma.vworld.PumaVirtualWorldMapper;
import org.cogchar.app.puma.web.PumaWebMapper;
import org.cogchar.app.puma.config.VanillaConfigManager;
import org.cogchar.platform.trigger.BoxSpace;
import org.cogchar.platform.trigger.CommandSpace;
import org.osgi.framework.BundleContext;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class PumaRegistryClientImpl implements PumaRegistryClient {
	// private	PumaAppContext			myAppContext;
	
	// Here are the 4 required ingredients of a PUMA application.  
	private PumaContextMediator		myMediator;
	private PumaConfigManager		myConfigManager;
	private	BoxSpace				myTargetBoxSpace;
	private	CommandSpace			myCommandSpace;
	
	// The remaining ingredients are all optional.
	//private PumaVirtualWorldMapper	myVWorldMapper;
	private PumaWebMapper			myWebMapper;	
	
	private BundleContext			myBundleContext;
	private ClassLoader				myInitialBonyRdfCL;
	// We now have a single instance of the web mapper here [via this.getWebMapper and PumaWebMapper.getWebMapper],
	// instead of separate instances for each PumaDualCharacter.
	
	public PumaRegistryClientImpl(BundleContext optBundleContext, PumaContextMediator mediator) {
		myBundleContext = optBundleContext;
		myMediator = mediator;
		myConfigManager = new VanillaConfigManager();
		myTargetBoxSpace = new BoxSpace();
		myCommandSpace = new CommandSpace();
		
		
	}

	@Override public PumaContextMediator getCtxMediator(Ident optSpecID) {
		return myMediator;
	}

	@Override public PumaConfigManager getConfigMgr(Ident optSpecID) {
		return myConfigManager;
	}

	@Override public BoxSpace getTargetBoxSpace(Ident optSpecID) {
		return myTargetBoxSpace;
	}

	@Override public CommandSpace getCommandSpace(Ident optSpecID) {
		return myCommandSpace;
	}

//	@Override public PumaVirtualWorldMapper getVWorldMapper(Ident optSpecID) {
//		return myVWorldMapper;
//	}
//
//	@Override public void putVWorldMapper(PumaVirtualWorldMapper vwm, Ident optSpecID) {
//		myVWorldMapper = vwm;
//	}

	@Override public PumaWebMapper getWebMapper(Ident optSpecID) {
		return myWebMapper;
	}

	@Override public void putWebMapper(PumaWebMapper wm, Ident optSpecID) {
		myWebMapper = wm;
	}

	@Override public List<ClassLoader> getResFileCLsForCat(ResourceFileCategory cat) {
		List<ClassLoader> extraCLs = myMediator.getExtraResFileCLsForCat(cat);
		List<ClassLoader> totalCLs = new ArrayList<ClassLoader>(extraCLs);
//		ClassLoader ourOpenGLResLoader = org.cogchar.bundle.render.resources.ResourceBundleActivator.class.getClassLoader();
//		totalCLs.add(ourOpenGLResLoader);
		return totalCLs;
	}
	
}
