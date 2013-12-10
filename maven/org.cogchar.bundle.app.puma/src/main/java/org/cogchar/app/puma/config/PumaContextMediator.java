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

import org.cogchar.app.puma.registry.PumaRegistryClient;
import java.util.ArrayList;
import java.util.List;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.matdat.RepoSpec;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.registry.ResourceFileCategory;

/**  This mediator is our "/etc/rc", the "top" mechanism available to customize the Cogchar PUMA boot up sequence.
 * @author Stu B. <www.texpedient.com>
 */
public abstract class PumaContextMediator extends BasicDebugger {

	/** Boot callback sequence #1.
	 *  The mediator should now do any special init that it wants to, but without assuming GUI exists.
		 * This stage includes getting the FIRST whack (both read and write) at the configuration services.  
		 * This should happen before the mediator is required to answer the getFlags and getMainConfigIdent,
		 * getModeIdent kinds of questions.
		 * However, the default mediator impl does nothing.  If you want the whole application to fail and
		 * shut down, you can signal that by allowing an exception to escape.
	 * @param ctx
	 * @throws Throwable 
	 */
	public void notifyContextBuilt(PumaAppContext ctx) throws Throwable {
	}
	/**  Boot callback sequence #2 - optional.
	 * Could apply to any GUI concepts, but currently applies only to our VWorld OpenGL panel.  This callback
	 * is currently *not* invoked if there is no VWorld startup (as controlled by VWorld flag below).
	 * Called after panels constructed but before startOpenGLCanvas. Allows GUI to intervene and realize the panels as
	 * needed. This step is normally used when getFlagAllowJFrames returns false, e.g. when we are running under NB
	 * platform and we want to manage windows manually). 
	 * 
	 * This callback comes after notifyContextBuilt, only if panels are actually constructed, as noted above.
	 * 
	 * If you want the whole application to fail and shut down, you can signal that by allowing an exception to escape.
	 * 
	 * @param ctx
	 * @throws Throwable
	 */	
	public void notifyPanelsConstructed(PumaAppContext ctx) throws Throwable {
	}
	/**  Boot callback sequence #3.
	 * The Puma chars have now been loaded, or reloaded.  If you want to fix up
	 * your chars as a group, this is a good opportunity to do so.  This might
	 * be called repeatedly if user re-loads the character system repeatedly,
	 * without a full reboot (and hence without the other callbacks necessarily
	 * repeating).
	 * 
	 * @param ctx
	 * @throws Throwable 
	 */
	public void notifyCharactersLoaded(PumaAppContext ctx) throws Throwable {
	}	
	/**
	 *  Boot callback sequence #4.
	 * Last thing that happens before result processing during a boot is this callback.
	 * Allows app to make sure it gets the last word on config + state.
	 * @param ctx
	 * @throws Throwable 
	 */
	public void notifyBeforeBootComplete(PumaAppContext ctx) throws Throwable {
		
	}

	public String getOptionalFilesysRoot() {
		return null;
	}


	public String getPanelKind() {
		return "SLIM";
	}
	public boolean getFlagAllowJFrames() {
		return true;
	}
	public boolean getFlagIncludeVirtualWorld() { 
		return true;
	}
	public boolean getFlagIncludeWebServices() { 
		return false;
	}
	public boolean getFlagIncludeCharacters() { 
		return true;
	}
	
	/**
	 * Used to set the URI of the PumaContextCommandBox, which is used as the most powerful/general target 
	 * in our "Cmd" models, which should match this URI for your application.
	 * @return 
	 */
	
	public String getSysContextRootURI() {
		String uriPrefix = "urn:ftd:cogchar.org:2012:runtime_instance#";
		String sysRootName = "pumaCtxCmdBox";
		String sysContextURI = uriPrefix + sysRootName;
		return sysContextURI;
	}

	/**
	 * Mediator determines 
	 * @return 
	 */
	public abstract RepoSpec getMainConfigRepoSpec();
	

	/** Synchronous callback allowing your app to be sure it has registered classLoaders
	 * that can see all required files in the different ResourceCategories.  In a simple JavaApp,
	 * these will probably just be the default classloader, and this callback is unimportant.
	 * But under OSGi, this method is a useful and easy way to get your classLoaders "in".
	 * 
	 * It is helpful to override this method with your own, because your application bundle
	 * has the best visibility into the set of classLoaders for other bundles.  Under OSGi,
	 * you can easily "import" the required packages, grab a sibing class, and ask for its
	 * classLoader.  Each classLoader should only be returned once, although repetitions
	 * should have little impact.  The ordering does not currently control any behavior, 
	 * but might be used later 
	 * 
	 * We intend that Puma should consult this method at least once for each category,
	 * before trying to load any resources from it.
	 * Puma may consult it multiple times, in which case Puma should not build up long
	 * lists of duplicate classloaders in any data structures!
	 * 
	 * [S2B22 - 2012-10-23].
	 * 
	 * @param cat
	 * @return 
	 */
	public List<ClassLoader> getExtraResFileCLsForCat(ResourceFileCategory cat) {
		return new ArrayList<ClassLoader>();
	}
}
