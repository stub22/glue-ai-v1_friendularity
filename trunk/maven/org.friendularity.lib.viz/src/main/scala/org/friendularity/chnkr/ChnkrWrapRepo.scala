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


class ChnkrWrapRepo(chunkDirMapper : ChnkrDirMapper)  extends BasicRepoImpl with FancyRepo   {
	private lazy val myCachedMappedDirModel = chunkDirMapper.getMappedDirModel
	
	private lazy val myMappedDSet : Dataset = {
		val dset = chunkDirMapper.getMappedDataset
		getLogger.info("Setting main query dataset for chunkWrapRepo to: {}", dset)
		setMainQueryDataset(dset)
		dset
	}
	override def getDirectoryModel() : JenaModel = {
		getLogger.info("mappedDset is {}", myMappedDSet)
		myCachedMappedDirModel
	}
	
	// TODO:  We must use some combination of dataset+model features from BasicRepoImpl to
	// achieve the correctly mapped repo state. 
	// Eventually we want to represent the dataset(s) facts using metadata at the GHost4 level.
	
	
}
class ChnkrWrapRepoSpec(brokerRecipeWrap: VWConfBrokerRecipeWrap, pathEntryHost : EntryHost) extends  RepoSpec { 

	// protected def makeChnker
	
	private lazy val myChunkHandle : LGSChunkHandle = {
		val chnkr = new VWConfContentChnkr(brokerRecipeWrap)
		val maxEntries = 100
		chnkr.buildChunkUsingSingleHost(pathEntryHost, maxEntries)			
	}
	
	private lazy val myCDM : ChnkrDirMapper = new ChnkrDirMapper(myChunkHandle)
		
	// Required override from RepoSpec
	override def makeDirectoryModel (): JenaModel = myCDM.getMappedDirModel

	// override protected def getChunkHandle : LGSChunkHandle = myChunkHandle
	
	protected def makeRepo(): org.appdapter.core.store.Repo.WithDirectory = {
		// val dirModel = makeDirectoryModel
		new ChnkrWrapRepo(myCDM)
	}
}