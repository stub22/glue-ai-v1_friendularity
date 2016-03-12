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
import com.hp.hpl.jena
import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.owrap.appro.{AGPProfileFragment, ASBRProfile}
import org.cogchar.api.owrap.crcp
import org.cogchar.api.owrap.crcp.GhRRefer
import org.cogchar.api.owrap.mdir.{GH4SFolder, GraphHost => MdirGraphHost, GraphPointer => MdirGraphPointer}
import org.cogchar.blob.chunk.{LGSChunkHandle, LoadableGraphState, TypedItemHandle}
import org.cogchar.blob.entry.EntryHost
import org.cogchar.blob.ghost.{GraphScanTest, RRUtil}
import org.ontoware.rdf2go
import org.ontoware.rdf2go.model.node.{URI => R2GoURI}
import org.ontoware.rdf2go.model.{Model => R2GoModel}


class ApproProfileGraphMaker(myPathEntryHost : EntryHost,	// Searchable folder host
						myProfileFolderPath : String,	// APPRO_FOLDER_PATH - relative to host
						myActiveTokens : Array[String])  // APPRO_ACTIVE_TOKENS
		extends VarargsLogging {
			
	val myMaxEntries : Int = 5000
	
	private lazy val myPBRW : ProfileBrokerRecipeWrap = makePBRW(myProfileFolderPath)
	private def makePBRW(profileFolderPath : String) : ProfileBrokerRecipeWrap = {
		// Create a RDF model in which the profile folder path is represented as a single folderGhost4,
		// which will be resolved against myPathEntryHost.
		val pbRecipeModelR2Go = GraphScanTest.makeEmptyTempR2GoModel
		val pbRecipe = new ASBRProfile(pbRecipeModelR2Go, true)
		
		val profileFolderGH4 = new GH4SFolder(pbRecipeModelR2Go, true)
		profileFolderGH4.setUrlText(myProfileFolderPath)
		val profileFolderRecipe = new GhRRefer(pbRecipeModelR2Go, true)
		profileFolderRecipe.setGraphHost(profileFolderGH4)
		// pbRecipe is in the mrcp: ontology.
		// To access methods in the Cogchar base type (which were generated in a different run of RDFReactor), 
		// we must "promote" pbRecipe to be of type crcp:BrokerRecipe.
		val ccrpView = RRUtil.promote(pbRecipe,classOf[crcp.BrokerRecipe])
		ccrpView.addInGhostRecipe(profileFolderRecipe)
		new ProfileBrokerRecipeWrap(pbRecipe)
		
	}
	private lazy val myBPChunker : ApproProfileChnkr = new ApproProfileChnkr(myPBRW, myActiveTokens)

	// All the MGPProfileFragment pointers in this chunk will be for active profile fragments.
	private lazy val myBPChunkHandle : LGSChunkHandle = myBPChunker.buildChunkUsingSingleHost(myPathEntryHost, myMaxEntries)
	
	// Produces a single merged profile graph, from all active profileFragments.
	// TODO:  Add smart consistency checks and override rules.
	def makeMergedProfileGraph : JenaModel = {
		info1("myBPChunkHandle={}  ", myBPChunkHandle)
		val mergedModel = JenaModelFactory.createDefaultModel
		val gpIndexModelR2Go : R2GoModel = myBPChunkHandle.myGPIndexModel
		val allGPs : Array[_ <: AGPProfileFragment] = AGPProfileFragment.getAllInstances_as(gpIndexModelR2Go).asArray
		val fragCount : java.lang.Integer = allGPs.length
		// Each gp is known to be from an *active* profile fragment.
		allGPs.foreach (gp => {
			val agppf : AGPProfileFragment = gp
			val gpURI  = agppf.asURI
			val llgsHandle : TypedItemHandle[LoadableGraphState] = myBPChunkHandle.getOrMakeLGStateHandle(gpURI)
			val lgs : LoadableGraphState = llgsHandle.unwrap
			val payModelR2Go  : Option[R2GoModel] = lgs.getOpenPayloadModel
			val payModelJena : JenaModel = payModelR2Go.get.getUnderlyingModelImplementation.asInstanceOf[JenaModel]
			info2("At gpURI {} found active profile-fragment payload model of size {}", gpURI, payModelJena.size : java.lang.Long)
			mergedModel.add(payModelJena)
		})		
		info2("Merged {} fragments to make single profile model of size: {}", fragCount, mergedModel.size : java.lang.Long)
		mergedModel
	}
}
