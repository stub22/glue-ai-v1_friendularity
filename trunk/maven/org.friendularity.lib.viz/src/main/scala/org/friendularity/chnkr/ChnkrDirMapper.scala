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

import org.cogchar.api.owrap.crcp
import crcp.{GhRRefer}

import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}
import rdf2go.model.node.impl.URIImpl

import org.cogchar.api.owrap.crcp

// trait CWrapFeatureConfig 
// trait CWrapVirtualWorldConfig
// trait CWrapGraphPointer

// This class connects our modern "chunk" to old-style repos used in avatar/cogchar configuration.
// We use it to load avatar+cogchar configs.
class ChnkrDirMapper(chunkHandle : LGSChunkHandle) extends VarargsLogging {
	protected def getChunkHandle : LGSChunkHandle = chunkHandle
	
	lazy val myInputDirModel_opt : Option[JenaModel] = getInputDirModelFromChunk
	
	private def getInputDirModelFromChunk : Option[JenaModel] = {
		val gpIdxModel : R2GoModel = getChunkHandle.myGPIndexModel 
		

		val dirGP_opt : Option[rdf2go.model.node.URI] = None
		if (dirGP_opt.isDefined) {
			val gpURI  = dirGP_opt.get.asURI
			// val llgsHandle : TypedItemHandle[LoadableGraphState] = chunkHandle.getOrMakeLGStateHandle(gpURI)
			// val lgs : LoadableGraphState = llgsHandle.unwrap
			// val payModelR2Go  : Option[R2GoModel] = lgs.getOpenPayloadModel
			val payModelJena : JenaModel = getJenaModelAtPointerURI(gpURI) // payModelR2Go.get.getUnderlyingModelImplementation.asInstanceOf[JenaModel]
			getLogger.info("At gpURI {} found 'dir.ttl' payload model of size {}", Seq(gpURI, payModelJena.size : java.lang.Long) : _*)
			Some(payModelJena)
		} else {
			getLogger.warn("Could not locate a 'dir.ttl' model")
			None
		}
			
		// val innerGP = new MdirGraphPointer(modelForPointerRec, vwconfPtrGP, false) // we are not writing this more general type, since it could be inferred.
		// innerGP.setPointsToGraphHost(hostToPointAt) 
	}
	private def getJenaModelAtPointerURI(gpURI : R2GoURI) : JenaModel = {
		val llgsHandle : TypedItemHandle[LoadableGraphState] = chunkHandle.getOrMakeLGStateHandle(gpURI)
		val lgs : LoadableGraphState = llgsHandle.unwrap
		val payModelR2Go  : Option[R2GoModel] = lgs.getOpenPayloadModel
		val payModelJena : JenaModel = payModelR2Go.get.getUnderlyingModelImplementation.asInstanceOf[JenaModel]
		payModelJena
	}
	private def getSourcePathForGP(innerGP : MdirGraphPointer) : String = {
		// val pointerModel = otherGP.getModel
		val chunkHandle = getChunkHandle
		
		// val innerGP = new MdirGraphPointer(pointerModel, otherGP.asURI, false)
		val ghostUri = innerGP.getAllPointsToGraphHost_as.asArray.head
		val srcGhostIdxModel : R2GoModel = chunkHandle.mySrcGHostIdxModel
		
		val ghostRec = new GraphHost3Triples(srcGhostIdxModel, ghostUri, false)
		ghostRec.getUrlText
	}
	lazy val myMappedMemDataset : Dataset = copyDirMetadataToChunkIndicesAndMakeMappedDataset
	
	def getMappedDataset : Dataset = myMappedMemDataset
	lazy val myMappedDirModel : JenaModel = {
		val dsetNames = myMappedMemDataset.listNames
		myInputDirModel_opt.getOrElse(JenaModelFactory.createDefaultModel)
	}
	def getMappedDirModel(): JenaModel = myMappedDirModel
// TODO:  Reconcile GraphPointer.hasGraphNameURI with dir.ttl entries like:
// rktsr:SpeechRecConfig
 //     rdf:type ccrt:FileModel , ccrt:SpeechRec ;
 //     ccrt:repo csi:filerepo_2014071107_5721_917 ;
 //     ccrt:sourcePath "SpeechRecConfig.ttl" .		
		


	private def copyDirMetadataToChunkIndicesAndMakeMappedDataset() : Dataset = {
//@prefix ccrt:    <urn:ftd:cogchar.org:2012:runtime#> .
// @prefix ccrti:   <urn:ftd:cogchar.org:2012:runtime_instance#> .		
// 
		val mappedMemDSet = jena.tdb.TDBFactory.createDataset()
		
		val nsCCRT = "urn:ftd:cogchar.org:2012:runtime#"
		val propUri_srcPath = nsCCRT + "sourcePath"
		val dirModelIn = myInputDirModel_opt.get
		val srcPathProp = dirModelIn.createProperty(propUri_srcPath)
		val srcPathStmts : StmtIterator = dirModelIn.listStatements(null, srcPathProp, null)
		while (srcPathStmts.hasNext) {
			val stmt = srcPathStmts.nextStatement
			val subj = stmt.getSubject
			val pathTxt = stmt.getString
			info2("Found source path relationship:  modelURI={}, sourcePath={}", subj, pathTxt)
			val matchingPointers = findPointersMatchingPathTail(pathTxt)
			if (matchingPointers.size == 1) {
				val matchingGP = matchingPointers.head
				val mdirGP = new MdirGraphPointer(matchingGP.getModel, matchingGP, false)
				val subjUriR2Go = new URIImpl(subj.getURI)
				mdirGP.setGraphNameUri(subjUriR2Go)
				val jm = getJenaModelAtPointerURI(matchingGP.asURI)
				mappedMemDSet.addNamedModel(subj.getURI, jm)
			} else {
				warn4("Found {} indexed models matching pathTail={}, for dir-entry-URI {}, pointers: {}", 
						matchingPointers.size : java.lang.Long, pathTxt, subj, matchingPointers)
			}
		}
		mappedMemDSet
	}	
	
	private def findPointersMatchingPathTail(pathTail : String) : Traversable[MdirGraphPointer] = {

		val gpIdxModel : R2GoModel = getChunkHandle.myGPIndexModel 
		val allGPs : Array[_ <: MdirGraphPointer] = MdirGraphPointer.getAllInstances_as(gpIdxModel).asArray
		allGPs.filter(gp => {
			val path = getSourcePathForGP(gp)
			path.endsWith(pathTail)
		})
	}
	
}




/*FancyRepoLoader
 *   def makeRepoWithDirectory(spec: RepoSpec, dirModel: Model, fileModelCLs: java.util.List[ClassLoader] = null, dirGraphID: Ident = null): DirectRepo = {
   
    var serial = System.identityHashCode(this);
    var repoDebugName = addInvisbleInfo(spec.toString, "time", "" + new Date());
    if (dirGraphID != null) {
      repoDebugName = addInvisbleInfo(repoDebugName, "id", "" + dirGraphID);
    }
    // Construct a repo around that directory        
	val repoBasePath = spec.getBasePath();
    val shRepo = new DirectRepo(spec, repoDebugName, repoBasePath, dirModel, fileModelCLs)
    //shRepo.beginLoading();
    // set to false to have concurrent background loading
    if (true) {
      //shRepo.finishLoading();
    }
	getLogger().info("Finished making RepoWithDirectory with debugName {}", repoDebugName)
    shRepo
  }
 */