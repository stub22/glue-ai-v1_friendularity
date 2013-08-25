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
package org.friendularity.api.goody;

import com.jme3.scene.Node;
import org.appdapter.core.name.Ident;
import org.appdapter.core.item.Item;

import org.appdapter.core.store.ModelClient;
import org.friendularity.math.api.MathGate;
import org.appdapter.core.log.BasicDebugger;
import org.cogchar.render.opengl.scene.DeepSceneMgr;
import org.cogchar.render.sys.registry.RenderRegistryClient;

import java.util.Set;
import java.util.HashSet;

/**
 *
 * @author Stu B. <www.texpedient.com>
 * 
 * Goal is flexibility + performance in animating fixed-size blocks V-World goodies using MathSpace and SpecGraphs
 * (which come from a user-editable source).  SpecGraph contains expressions which populate the MathSpace.
 * SpecGraph also defines instructions for how to construct + apply updates to particular goodies.
 * 
 */


public class DynamicGoodySpace extends BasicDebugger {
	
	private	Ident			mySpecGraphID, mySpecID;
	private	ModelClient		myCachedModelClient;
	private	MathGate		myMathGate;
	private	Integer			myGoodyCount;
	
	private	Node			myGroupDisplayNode, myParentDisplayNode;
	//  We use an array to emphasize the indexed nature of this space.
	// However, "goodyIndex" always starts at 1, so we have to subtract 1
	private	DynamicGoody	myGoodies[] = new DynamicGoody[0];
	
	public DynamicGoodySpace(Ident specGraphID, Ident specID) { 
		mySpecGraphID = specGraphID;
		mySpecID = specID;
		myGroupDisplayNode = new Node(specGraphID.getLocalName());
	}
	public Ident getSpecGraphID() {
		return mySpecGraphID;
	}
	
	public DynamicGoody getGoodyAtIndex(int oneBasedIndex) {
		if ((oneBasedIndex >= 1) && (oneBasedIndex <= myGoodies.length)) {
			return myGoodies[oneBasedIndex - 1];
		} else {
			getLogger().error("Supplied index {} is out of bounds [1, {}]", oneBasedIndex, myGoodies.length);
			throw new RuntimeException("out of bounds goody index sent to space: " + mySpecGraphID);
		}
	}
	public Ident mapGoodyIndexToID(Integer idx) {
		return null;
	}
	public void refreshMathGate(MathGate mg) {
		myMathGate = mg;
	}	
	public void refreshModelClient(ModelClient mc) {
		myCachedModelClient = mc;
		Item specItem = mc.makeItemForIdent(mySpecID);
		getLogger().info("Got space-specItem: {}", specItem);
		String goodyCount_Prop_QN = "hev:goodyCount";
		Ident goodyCount_Prop_ID = mc.makeIdentForQName(goodyCount_Prop_QN);
		Integer goodyCount = specItem.getValInteger(goodyCount_Prop_ID, 0);
		getLogger().info("goodyCount=" + goodyCount);
		resizeSpace(goodyCount);
		
		// TODO:  Reload Stuff
	}
	public Set<Item> getGoodySpecItems() { 
		Set<Item> specItems = new HashSet<Item>();
		return specItems;
	}
	public void setParentDisplayNode_onRendThrd(Node n) { 
		// create and/or reparent the GroupDisplayNode
		myParentDisplayNode = n;
		myParentDisplayNode.attachChild(myGroupDisplayNode);
	}
	public void attachChildDisplayNodes_onRendThrd() { 
		// For all goodies, attach the child node to our groupDisplayNode
	}
	/*
	public void a_onRendThrd(RenderRegistryClient	rrc) {
		if (mySubsysNode != null) {
			DeepSceneMgr dsm = rrc.getSceneDeepFacade(null);
			dsm.attachTopSpatial(mySubsysNode);
		}
	}	
	*/
	
	public void doFastVWorldUpdate() { 
		for (int idx = 0; idx < myGoodies.length; idx++) {
			myGoodies[idx].doFastVWorldUpdate();
		}
	}
	
	/**
	 * The only way to create or destroy goodies is to resize the space (which is usually done only by updating 
	 * the space-level spec).
	 * On expansion, existing goodies survive.  On contraction, all goodies
	 * up to the new size survive, higher than that size are forgotten.
	 * @param size 
	 */
	public void resizeSpace(int size) { 
		int oldSize = myGoodies.length;
		if (oldSize == size) {
			return;
		}
		DynamicGoody nGoodies[] = new DynamicGoody[size];
		int maxCopy = Math.min(oldSize, size);
		for (int idx =0; idx < maxCopy; idx++) {
			nGoodies[idx] = myGoodies[idx];
		}
		for (int jdx = maxCopy; jdx < size; jdx++) {
			// jdx is zero-based, so we add one to set the 1-based Goody-space index.
			nGoodies[jdx] = new DynamicGoody(jdx + 1);
		}
		myGoodies = nGoodies;
	}

}
