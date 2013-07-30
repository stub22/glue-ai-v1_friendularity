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
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.item.Item;
import org.appdapter.core.store.ModelClient;
//import org.appdapter.help.repo.{RepoClient, RepoClientImpl, InitialBindingImpl} 
//import org.appdapter.impl.store.{FancyRepo};
//import org.appdapter.core.matdat.{SheetRepo,_}
//import com.hp.hpl.jena.query.{QuerySolution} // Query, QueryFactory, QueryExecution, QueryExecutionFactory, , QuerySolutionMap, Syntax};
import com.hp.hpl.jena.rdf.model.Model;

import org.appdapter.core.log.BasicDebugger;

/**
 *
  * @author Stu B. <www.texpedient.com>
  * 
  * Manages a single goody that is part of some space.  (For now, we assume there is just one).
  * A Goody has a presence in all three of:
  *		1) Semantic Space: a GoodySpec, which may be edited by user
  *		2) Math Space : a set of parameters changing over time, related by functions, as defined by specs
  *		3) V-World OpenGL Space: rendered model display for user
  * 
  * A Goody has an index.  The first goody is at 1 (not 0).
 */


public class DynamicGoody extends BasicDebugger {
	public static class Spec {
		Ident	myKind;
	}
	
	private DynamicGoodySpace		myDGSpace;
	private	Node					myDisplayNode;
	private	Integer					myGoodyIndex;
	private	Ident					myGoodyID;

	protected DynamicGoody(int index) {
		myGoodyIndex = index;
	}

	public void setSpace(DynamicGoodySpace dgSpace) {
		myDGSpace = dgSpace;
	}
	protected void reloadConfig(ModelClient mc) {
		
	}
	public void doFastVWorldUpdate() { 
		
	}
	public void updateStuff(ModelClient mc) { 
		Ident	goodyID = myGoodyID;
	}
	// Currently we need the ModelClient to resolve QNames to idents.
	public void updateFromSpecItem(ModelClient mc, Item specItem) {
		
		// TODO:  These property names need to come from ontology-generated constants.
		String posVecExpr_Prop_QN = "hev:expr_pos_vec3f";
		String colorVecExpr_Prop_QN = "hev:expr_color_vec4f";
		// This resolution (and hence direct dependence on ModelClient mc) could be done "once", on a per-space or global basis.
		Ident posVecExpr_Prop_ID = mc.makeIdentForQName(posVecExpr_Prop_QN);
		Ident colorVecExpr_Prop_ID = mc.makeIdentForQName(colorVecExpr_Prop_QN);
		
		// From here on out, we only use Items + Idents (so we're not directly RDF/Jena dependent).
		String posVecExpr = specItem.getValString(posVecExpr_Prop_ID, "{0, 0, 0}");		
		String colorVecExpr = specItem.getValString(colorVecExpr_Prop_ID, "{0, 0, 0, 0}");
		getLogger().info("posVecExpr=" + posVecExpr + ", colorVecExpr=" + colorVecExpr);
	}
	/* public void readSpec
	public Ident getIdent() { 
		if (myGoodyID = null) {
			String qName = "h"
			myGoodyID = new 
		}
	}
	*/
}
