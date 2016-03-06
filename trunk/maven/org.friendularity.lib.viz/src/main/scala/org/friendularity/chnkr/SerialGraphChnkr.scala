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

import java.io.File

import com.hp.hpl.jena
import org.apache.jena.riot.RDFDataMgr

import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }
import jena.ontology.Individual
import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}

import rdfreactor.runtime.ReactorRuntimeEntity
import rdfreactor.schema.rdfs.{Class => RDFR_Class}

import org.appdapter.fancy.log.VarargsLogging

import org.cogchar.api.owrap.mdir.{GH4SFolder, GraphHost3Serial, GraphHost3Triples}

import org.cogchar.api.owrap.mdir.{GraphPointer => MdirGraphPointer, GraphHost => MdirGraphHost}

import org.cogchar.blob.circus.{ BrokerRecipeWrap, FeatureBrokerRecipeWrap, BrokerRecipeUtil, CircusTest}
import org.cogchar.blob.ghost.{GraphScanTest, IndexResult,  RRUtil, GHostUtil, GHostRecipeWrap}
import org.cogchar.blob.entry.{EntryHost, PlainEntry, FolderEntry}
import org.cogchar.blob.chunk.{LGSChunkHandle, TypedItemHandle, LoadableGraphState, LoadableGraphHandleFuncs, RootChunkHandle, HasR2GoURI}


// import ChnkrGraphPointer 
	


// This chunker requires 2 methods from subclasses
// 1) getBrokerRecipeWrap   
// 2) makeGraphPointerRec
abstract class SerialGraphChnkr extends VarargsLogging  {
	def getBrokerRecipeWrap : BrokerRecipeWrap
	
	// Returns a collection of folder-descriptions found in our recipe data.
	def findInputGhost4Folders : Traversable[GH4SFolder] = {
		val brw = getBrokerRecipeWrap
		val contentGHostFolders  : Traversable[GH4SFolder] = brw.findInputDirectGHost4Folders
		contentGHostFolders
	}
	// This does an app-specific index.  Each input GHost record is used to locate the actual graph data,
	// which we then probe for interesting contents.   The interesting information is recorded as an
	// app-specific  GraphPointer, in the argument graphPointerModel.
	def indexSerial3Content(serial3GHosts : Traversable[GraphHost3Serial], graphPointerModel : R2GoModel) : IndexResult = {
		// Make our app-specific pointer-building function, which looks for interesting info in a given R2GoModel, and
		// returns it as an Optional graphPointer.  If we think this graph is uninteresting, we can return None.
		val gpMaker : Function2[R2GoModel, GraphHost3Serial, Option[MdirGraphPointer]] = (cmod, gh3s) => {
			val pointer_opt : Option[MdirGraphPointer] = makeGraphPointerRec(graphPointerModel, gh3s, cmod)
			pointer_opt.map(RRUtil.promote(_, classOf[MdirGraphPointer]))
		}
		// Ask Cogchar utility method to do the index for us, using our special gpMaker function.
		GHostUtil.indexGH3Serials(serial3GHosts, gpMaker)
	}
	
	// Override this method to make appropriate graph pointers.  Return None when no pointer is desired.
	protected def makeGraphPointerRec(modelForPointerRec : R2GoModel, hostToPointAt : GraphHost3Triples, 
									  dataModel : R2GoModel) : Option[MdirGraphPointer] 
		
	// Here is one decent way to build an LGS-chunk, but it is not the only way.
	// If you want to experiment with changes to this code, you can start with hacking the similar stuff in 
	// TestOtherScan.testContentFolderScanAndIndex.
	// 
	def buildChunkUsingSingleHost(dataEntryHost : EntryHost, maxEntries : Int) : LGSChunkHandle = {
		// 1) Use the given chunker to find the relative pathnames of some folders to recursively search.
		// This tells us *nothing* about what entryHosts to search in.  This just gives us RDF descriptions
		// of the search-root folders, which each contains a URL string (which we presume to be relative to
		// some given entryHost).   
		val contentGH4Folders : Traversable[GH4SFolder]  = findInputGhost4Folders 
		info1("Found contentGH4Folders: {}", contentGH4Folders)
		
		// 2) Make empty models to hold the two results summaries.
		// It is also possible (but not recommended) to use just one graph for both kinds of summary.
		val ghRecModel: R2GoModel = GraphScanTest.makeEmptyTempR2GoModel  // holds ghostRecs = physical graph paths
		val graphPointerModel : R2GoModel = GraphScanTest.makeEmptyTempR2GoModel  // holds gPointers = content summaries

		// 3) "scan" - Here is the folder scanning step, where we discover pathnames of graph files in a given entry host.
		// We recursively search all contents of each of the contentGH4Folders (each of which has
		// a relative folder path) found above in step 1.
		// Each search hit (graph file path) is recorded in ghRecModel as an instance of GraphHost3Serial, and those 
		// are also returned as a collection.  This ghRecModel is kept by the chunk, and used by each payload-handle,
		// whenever we want to find the given graph pathname.  
		val serial3GHosts : Traversable[GraphHost3Serial] = 
				GHostUtil.recordGH3SerialsFoundInGH4Folders(contentGH4Folders, dataEntryHost, maxEntries, ghRecModel)
		
		// 4) "index" - Now we read the graph files and store summary info about them in graphPointerModel.
		val idxResult : IndexResult = indexSerial3Content(serial3GHosts, graphPointerModel)

		// Print out the summary information, in a few different ways.
		trace1("Pointer model contents: {}", graphPointerModel.getUnderlyingModelImplementation)
		// printIndexedContentInfo(graphPointerModel)
	
		// 5) "chunk" - Now that we have summary models, we are in position to build a caching chunk.
		val rootChunkHandleURI = null  // This URI isn't important
		val rootChunkHandle = new RootChunkHandle(rootChunkHandleURI) // This root chunk doesn't do anything useful yet.
		val lgsChunkHandleURI = null // This URI doesn't matter.

		// Sets up an initially empty handle-cache, and returns a chunk-handle to it.
		// A handle for each payload graph will be created only when we ask for it (by supplying a graphPointerURI).
		// So, this chunk creation is a reasonably lightweight operation (once the summary models are already built).
		val lgsChunkHandle : LGSChunkHandle = LoadableGraphHandleFuncs.makeChunkForLoadableGraphs(lgsChunkHandleURI, 
					rootChunkHandle, graphPointerModel, ghRecModel)
		
		info1("Built LGSChunkHandle: {}", lgsChunkHandle)
		lgsChunkHandle
		
	}	
}
