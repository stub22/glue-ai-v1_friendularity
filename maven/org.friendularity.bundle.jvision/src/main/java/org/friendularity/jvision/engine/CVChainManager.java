/*
*  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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
package org.friendularity.jvision.engine;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import org.friendularity.jvision.gui.CVChainControl;
import org.friendularity.jvision.gui.FilterBox;
import org.slf4j.LoggerFactory;

import java.util.HashMap;

/**
 * Singleton that acts as a builder for new CVChains and their controls
 *
 * @author Annie
 * @see CVChain for a discussion of CVChains
 *
 * This is the coordinator for CVChains - it makes sure they have system wide unique names, and
 * handles wiring up CVChainControl, the FilterBox, and the CVChain model
 */
public class CVChainManager {
	private static CVChainManager defaultCVManager = null;

	private HashMap<String, CVChain> cvChains = new HashMap<>();

	public static CVChainManager getDefaultManager() {
		if (defaultCVManager == null)
			defaultCVManager = new CVChainManager();

		return defaultCVManager;
	}

	private CVChainManager() {

	}

	/**
	 * Is it OK to make a chain with this name?
	 */
	public boolean chainExists(String name) {
		return cvChains.containsKey(name);
	}

	/**
	 * try to create a new CVChain with no filters in it, and add a CVChainControl to
	 * the FilterBox
	 */
	public void buildChain(FilterBox fb, String name, boolean intermediatesVisible, String source) {
		if (chainExists(name))
			throw new IllegalArgumentException("Chain " + name + " already exists");

		CVChain cvchain = new CVChain(name, intermediatesVisible, source);
		cvChains.put(name, cvchain);
		CVChainControl cvc = new CVChainControl(cvchain, fb);

		fb.addCVChainControl(cvc);
	}

	public void buildChain(FilterBox fb, Resource cvchainRDF, Model M) {
		CVChain cvchain = new CVChain(M, cvchainRDF);
		cvChains.put(cvchain.getName(), cvchain);
		CVChainControl cvc = new CVChainControl(cvchain, fb);
		fb.addCVChainControl(cvc);
	}

	/**
	 * famulus to FilterBox, the manager takes care of coordinating the pieces
	 * when user removes a CVChain
	 *
	 * @param fb   the FilterBox we're in
	 * @param cvcc the control to remove
	 */
	public void remove(FilterBox fb, CVChainControl cvcc) {
		if (fb == null || cvcc == null) {
			LoggerFactory.getLogger(CVChainManager.class.getName()).error(
					"evil thing in remove in CVChainManager");
		}
		fb.removeChainControl(cvcc);
		CVChain cvc = cvChains.get(cvcc.getName());
		cvc.unwire();
		cvChains.remove(cvcc.getName());
	}


}
