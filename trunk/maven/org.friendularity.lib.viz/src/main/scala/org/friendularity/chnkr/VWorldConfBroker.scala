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

package org.friendularity.chnkr
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.core.log.{ BasicDebugger, Loggable }
import org.appdapter.fancy.rspec.RepoSpec
import org.appdapter.core.store.{ Repo, BasicRepoImpl }
import org.appdapter.fancy.repo.{FancyRepo}
import com.hp.hpl.jena
import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory, StmtIterator, Property, Statement, Resource }
import jena.query.{Dataset}
import org.cogchar.blob.entry.{EntryHost, PlainEntry, FolderEntry}

import org.cogchar.blob.chunk.{LGSChunkHandle, TypedItemHandle, LoadableGraphState, LoadableGraphHandleFuncs, RootChunkHandle, HasR2GoURI}
import org.cogchar.blob.circus.{ FeatureBrokerRecipeWrap, BrokerRecipeWrap, BrokerRecipeUtil}
import org.cogchar.api.owrap.mdir.{GH4SFolder, GraphHost3Serial, GraphHost3Triples}

import org.cogchar.api.owrap.mdir.{GraphPointer => MdirGraphPointer, GraphHost => MdirGraphHost}

import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}
import rdf2go.model.node.impl.URIImpl

import org.cogchar.api.owrap.crcp.{BRFeature}

// Accepts a general "Config" Broker recipe, which we presume leads to a ghostRecipe for config data for vworld.
class VWConfBrokerRecipeWrap(val configBR : BRFeature) extends 
		FeatureBrokerRecipeWrap(BrokerRecipeUtil.toFeatureBrokerRecipe(configBR)) {
}


class VWConfContentChnkr(private val myBRW : VWConfBrokerRecipeWrap)  extends SerialGraphChnkr { 

	override def getBrokerRecipeWrap : BrokerRecipeWrap = myBRW
   
	override protected def makeGraphPointerRec(modelForPointerRec : R2GoModel, hostToPointAt : GraphHost3Triples, dataModel : R2GoModel) : Option[MdirGraphPointer]  = {
		val innerGP = new MdirGraphPointer(modelForPointerRec, true)
		innerGP.setPointsToGraphHost(hostToPointAt)
		Some(innerGP)
	}
}