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

package org.friendularity.bindx

/**
 * @author Stu B. <www.texpedient.com>
 * 
 *  : class XMLLiteral
location: package org.ontoware.rdfreactor.schema.rdfs

 
 */



import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.log.BasicDebugger;

trait CheckoutConn {
	def	makeCheckoutHandle(graphURI : Ident) : ModelCheckoutHandle
	
}
trait ModelCheckoutHandle {
	def getGraphIdent : Ident
	def refreshCheckout 
	def checkinAsAdd
	def checkinAsReplace
	def deleteGraph
	def getAsReactorModel : org.ontoware.rdf2go.model.Model;
}

import com.hp.hpl.jena.query.{DatasetAccessor, Dataset, DatasetAccessorFactory}
import com.hp.hpl.jena.rdf.model.{Model, ModelFactory}

class JenaArqCheckoutConn(private val myDsetAcc : DatasetAccessor) extends CheckoutConn {
	
	def getJenaModel(graphId : Ident) : Model = {
		val graphAbsUri = graphId.getAbsUriString
		// Problem:  From code inspection it appears that this model is local (not a copy)
		myDsetAcc.getModel(graphAbsUri)
	}
	def putJenaModel(graphId : Ident, jm : Model) {
		val graphAbsUri = graphId.getAbsUriString
		myDsetAcc.putModel(graphAbsUri, jm)
	}
	def postJenaModel(graphId : Ident, jm : Model) {
		val graphAbsUri = graphId.getAbsUriString
		myDsetAcc.add(graphAbsUri, jm)		
	}
	def deleteJenaGraph(graphId : Ident) {
		val graphAbsUri = graphId.getAbsUriString
		myDsetAcc.deleteModel(graphAbsUri)	
	}
	// This always makes a separate checkout.
	// TODO:  Consider keeping track of existing checkouts, sharing them among callers
	override def makeCheckoutHandle(graphURI : Ident) : ModelCheckoutHandle = {
		new JenaModelCheckoutHandle(graphURI, this)
	}
}
/**
 * Uses a private in-memory model to hold contents of the checkout.
 * Chose this design because it appears that DatasetAccessor for a *local* dataset returns a modifiable
 * model, which does not fit the idea of "checkout".   So, we go ahead and make sure there is always a
 * local copy, i.e. the checkout, and build up conceptually from there (even tho it means we might make
 * an unnecessary extra copy in the case that the conn is actually remote, which already implies copying).
 */
case class JenaModelCheckoutHandle(private val myGraphId : Ident, private val myConn : JenaArqCheckoutConn) 
		extends BasicDebugger with ModelCheckoutHandle {
			
	val myLocalModel : Model = ModelFactory.createDefaultModel
	lazy val myReactorModel = new org.ontoware.rdf2go.impl.jena.ModelImplJena(myLocalModel)

	override def getGraphIdent = myGraphId
	
	override def refreshCheckout {
		val fromConn : Model = myConn.getJenaModel(myGraphId)
		myLocalModel.removeAll() 
		if (fromConn != null) {
			getLogger().info("Adding {} triples from retrieved model {} to local checkout copy", fromConn.size, myGraphId)
			myLocalModel.add(fromConn)
		} else {
			getLogger().info("No model found at {}, starting from empty local checkout model", myGraphId)
		}
	}
	override def checkinAsAdd {
		getLogger().info("Posting {} triples from local checkout copy to {} - as ADD", myLocalModel.size, myGraphId)
		myConn.postJenaModel(myGraphId, myLocalModel)
		getLogger.info("Finished posting ADD")
	}
	override def checkinAsReplace {
		getLogger().info("Posting {} triples from local checkout copy to {} - as REPLACE", myLocalModel.size, myGraphId)
		myConn.putJenaModel(myGraphId, myLocalModel)
		getLogger.info("Finished posting REPLACE")
	}
	override def deleteGraph {
		getLogger().info("Deleting remote jenaGraph at {}, *without* clearing local model", myGraphId)		
		myConn.deleteJenaGraph(myGraphId)
		getLogger.info("Finished DELETE {}", myGraphId)
	}
	override def getAsReactorModel = myReactorModel
	
}

