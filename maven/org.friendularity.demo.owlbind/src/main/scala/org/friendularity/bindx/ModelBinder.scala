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
 */

trait ModelBinder {
	import org.ontoware.rdfreactor.runtime.Base;
	
	def getR2goModel : org.ontoware.rdf2go.model.Model
	def getSingleBoundObj [BT <: org.ontoware.rdfreactor.schema.rdfs.Class](r : org.ontoware.rdf2go.model.node.Resource, 
						p : org.ontoware.rdf2go.model.node.URI, desiredClass : Class[BT]): Option[BT] = {
					
		val r2goModel = getR2goModel
		val found : BT = Base.get(r2goModel, r, p, desiredClass).asInstanceOf[BT]
		if (found != null) {
			Some(found)
		} else {
			None
		}
	}		
	
}
class ModelBinderChecky(val myCheckoutHandle : ModelCheckoutHandle) extends ModelBinder {
	lazy val myR2goModel : org.ontoware.rdf2go.model.Model = myCheckoutHandle.getAsReactorModel
	override def getR2goModel : org.ontoware.rdf2go.model.Model = myR2goModel
	
	def freshen() {
		// What would be effects of  myR2goModel.close()  ?
		myCheckoutHandle.refreshCheckout
		myR2goModel.open() // OK to open a second time?
	}
	def checkinAsReplace() {
		myCheckoutHandle.checkinAsReplace;
	}	
}
	
class MDirBinder(checkHandle :  ModelCheckoutHandle) extends ModelBinderChecky(checkHandle) {
	import org.friendularity.gen.reacted.mdir._
	import org.ontoware.rdf2go.model.node.Resource
	import org.ontoware.rdf2go.model.node.URI
	import org.ontoware.rdf2go.model.node.impl.URIImpl
	
	val ns_gmdinst = "urn:fdc:friendularity.org:2014:gmdinst#"
	
	val fragPrefix_gptr = "gptr."
	val fragPrefix_gptrOpen =  fragPrefix_gptr + "open."	
	
	def makeURI(absUriText : String) : URI = new URIImpl(absUriText)
	def makeInstanceURI(fragTail : String) : URI = makeURI(ns_gmdinst + fragTail)
	def makeGPOpenUri(fragTail : String) = makeInstanceURI(fragPrefix_gptrOpen + fragTail)
	
	def findHost5FusekiServer(ghost5Res : Resource) : GH5RSFusekiServer = {
		freshen()
		// Failure will return an empty, unasserted server object.
		val finderObj = new GH5RSFusekiServer(myR2goModel, ghost5Res, false);		
		finderObj
	}
	def ensureHost5FusekiServer(ghost5Res : Resource, serverUrlText : String) : GH5RSFusekiServer = {
		freshen()
		val ensuredObj = new GH5RSFusekiServer(myR2goModel, ghost5Res, true);
		ensuredObj.setUrlText(serverUrlText)
		checkinAsReplace()
		ensuredObj
	}
	def findHost4FusekiDataset(ghost4Res : Resource) : GH4RSOHFusekiDataset = {
		freshen()
		// Failure will return an empty, unasserted server object.
		val finderObj = new GH4RSOHFusekiDataset(myR2goModel, ghost4Res, false);		
		finderObj		
	}

	def ensureHost4FusekiDataset(ghost4Res : Resource, ghost5Res : Resource, ghost4ServiceUrl : String) : GH4RSOHFusekiDataset = {
		val ghost5server = findHost5FusekiServer(ghost5Res) // dirty secret for now: implies freshen
		val ensuredDSet = new GH4RSOHFusekiDataset(myR2goModel, ghost4Res, true);
		ensuredDSet.setParentHost5Quints(ghost5server)
		ensuredDSet.setUrlText(ghost4ServiceUrl)
		checkinAsReplace()
		ensuredDSet
	}
	
	//def ensureGPOpenToGHost4(gpUriFragTail : String, ghost4UriText : String, graphNameUri : String) : GPOpen = {	
	//}
}

