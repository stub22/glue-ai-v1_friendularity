/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.appro

import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}

import com.hp.hpl.jena
import org.apache.jena.riot.RDFDataMgr

import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }

import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.core.name.{ FreeIdent, Ident }

import org.cogchar.api.owrap.crcp
import crcp.{GhRRefer}
import org.cogchar.blob.circus.{ BrokerRecipeUtil, BrokerRecipeWrap}
import org.cogchar.api.owrap.mdir.{GH4SFolder, GraphHost3Serial, GraphHost3Triples}
import org.cogchar.api.owrap.mdir.{GraphPointer => MdirGraphPointer, GraphHost => MdirGraphHost}
import org.cogchar.api.owrap.appro.{ApproGraphPointer, AGPProfileFragment, ASBRProfile }
import org.cogchar.blob.chunk.{LGSChunkHandle, TypedItemHandle, LoadableGraphState, LoadableGraphHandleFuncs, RootChunkHandle, HasR2GoURI}
import org.cogchar.blob.ghost.{RdfNodeUtils, RRUtil, GraphScanTest}
import org.cogchar.blob.entry.{EntryHost}

import org.friendularity.chnkr.SerialGraphChnkr

// Apply app-profile ("appro") graph chunk for app root recipes + settings, with a set of source subfolders switchable 
// by cmd-args or system prop/env 

class ProfileBrokerRecipeWrap(pbr : ASBRProfile) extends BrokerRecipeWrap(RRUtil.promote(pbr,classOf[crcp.BrokerRecipe])) {
}


class ApproProfileChnkr(private val myPBRW : ProfileBrokerRecipeWrap, private val myActiveTokens : Array[String]) extends SerialGraphChnkr {
	
	val myTokensSet = myActiveTokens.toSet
	
	override def getBrokerRecipeWrap : BrokerRecipeWrap = myPBRW

	override protected def makeGraphPointerRec(modelForPointerRec : R2GoModel, hostToPointAt : GraphHost3Triples, 
											   dataModel : R2GoModel) : Option[MdirGraphPointer]  = {
		val profileFragGP = makeProfileFragGraphPointerRec(modelForPointerRec, hostToPointAt, dataModel)
		val mdirGraphPtr = profileFragGP.map(new MdirGraphPointer(modelForPointerRec, _, false))
		mdirGraphPtr
	}
	private def makeProfileFragGraphPointerRec(modelForPointerRec : R2GoModel, hostToPointAt : GraphHost3Triples, 
											   dataModel : R2GoModel) : Option[AGPProfileFragment]  = {
		
		val hostUrlTxt = hostToPointAt.getUrlText
		val pathStrings = hostUrlTxt.split("/")
		debug2("Possible profile fragment graph found at {}, with pathStrings: {}", hostUrlTxt, pathStrings)
		if (pathStrings.length >= 2) {
			val keyFolderName = pathStrings(pathStrings.length - 2)
			val keyStrings = keyFolderName.split("_")
			debug3("Split keyFolderName: {} into keys: {} to match with active: {}", keyFolderName, keyStrings, myTokensSet)
			// All keys must match in myActiveTokens, currently case-sensitive.
			val keysSet = keyStrings.toSet
			val keysCount : java.lang.Integer = keyStrings.size
			val intersect = keysSet.intersect(myTokensSet)
			val intersectCount : java.lang.Integer = intersect.size
			debug2("Matched {} out of {} path keys as active tokens", intersectCount, keysCount)
			if (intersectCount == keysCount) {
				info4("All {} keys in {} matched, activating profile graph fragment pointer at {} for URL {}", 
						keysCount, keyStrings, hostToPointAt, hostUrlTxt)
				// In this baseline impl, we simply merge all the matching graphs.
				// TODO:  Check for graph conflicts, and resolve using specificity 
				// so a property set under "desk_r25_wacko" should win over one set in "desk_r25" or "r25",
				// assuming activeTokens contains all 3 entries, e.g. "avatar r25 wacko desk".
				val profileFragGP = new AGPProfileFragment(modelForPointerRec, true)
				val innerGP = new MdirGraphPointer(modelForPointerRec, profileFragGP, false) // we are not writing this more general type, since it could be inferred.
				innerGP.setPointsToGraphHost(hostToPointAt)
				Some(profileFragGP)
			} else { 
				// Extra debug: print the keys not matched
				val unmatched = keysSet.diff(myTokensSet)
				val expectedDiffs : java.lang.Integer = keysCount - intersectCount
				val actualDiffs : java.lang.Integer = unmatched.size
				debug3("The following {}=?={} keys were not matched: {}", actualDiffs, expectedDiffs, unmatched)
			}
		} else {
			warn3("Only {} (< required 2) tokens found in {}, tokens: {}", pathStrings.length : java.lang.Integer, hostUrlTxt, pathStrings)
		}
		None
		
	}	
	
}

