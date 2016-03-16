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

import org.appdapter.fancy.log.VarargsLogging;

import com.hp.hpl.jena
import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }
import org.friendularity.chnkr.{ChnkrWrapRepo, AvatarSetupFuncs}


import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}


import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.core.name.{ FreeIdent, Ident }

import org.cogchar.blob.ghost.{GraphScanTest, IndexResult,  RRUtil, GHostUtil, GHostRecipeWrap}
import org.cogchar.blob.entry.{EntryHost, PlainEntry, FolderEntry, DiskEntryHost, ResourceEntryHost}


import org.cogchar.api.owrap.crcp.{BRFeature => CC_BRFeature}
import org.cogchar.api.owrap.appro.AFBRLegacyConfig

object TestRaizLoad extends AvatarSetupFuncs with  VarargsLogging {
	val vizappRecipeNS = "http://onto.friendularity.org/indiv/vizappRecipes_reg_desk_2016Q1#"
	val vizappBrokerRecipeUriTxt = vizappRecipeNS + "vizapp_legConf_brokerRecipe"
	val pathToProfileFolder = "org/friendu/tchunk/vizapp_profile"

	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
		//	org.apache.log4j.BasicConfigurator.configure();
		//	org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		//
		//	Appears that currently Akka is automatically initing logging with our log4j.properties.

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestRaizLoad main().START");

		//

		// GraphScanTest.setupScanTestLogging
		info0("Starting TestProfileLoad")

		val mergedProfileGraph = getMergedProfileGraph_RegularDesktop
		info1("Fetched mergedProfileGraph of size {}", mergedProfileGraph.size : java.lang.Long)
		debug1("mergedProfileGraph dump:\n{}", mergedProfileGraph)

		wow(mergedProfileGraph)
	}
	def getMergedProfileGraph_RegularDesktop : JenaModel = {
		val activeTokens = Array[String]("all", "regular", "desktop")
		getMergedProfileGraph(activeTokens)
	}
	def getMergedProfileGraph(activeTokens : Array[String]) : JenaModel = {
		val profDataEntryHost : EntryHost = getUnitTestResourceEntryHost

		val pgm = new ApproProfileGraphMaker(profDataEntryHost, pathToProfileFolder,  activeTokens)

		val mergedProfileGraph : JenaModel = pgm.makeMergedProfileGraph
		mergedProfileGraph
	}


	def  getUnitTestResourceEntryHost : EntryHost = {
		val dataMarkerClazz : java.lang.Class[_] = classOf[ApproRaizCtxImpl]
		val dataEntryHost : EntryHost = new ResourceEntryHost(dataMarkerClazz)
		dataEntryHost
	}

	def wow(profileJM : JenaModel) : Unit = {

		val bootRecipesR2Go = open4R2go(profileJM)

		val legConfigBR = new CC_BRFeature(bootRecipesR2Go, vizappBrokerRecipeUriTxt, false)
					// new AFBRLegacyConfig(bootRecipesR2Go, vizappBrokerRecipeUriTxt, false)


		val cpathEntryHost: EntryHost = getUnitTestResourceEntryHost

		val cwRepoSpec = makeVWConfRepoSpec(legConfigBR, cpathEntryHost)
		val cwRepo = cwRepoSpec.getOrMakeRepo.asInstanceOf[ChnkrWrapRepo]

		val dirModel = cwRepo.getDirectoryModel()

		info1("Fetched repo dir model: {}", dirModel)

		java.lang.Thread.sleep(3000)
		info0("That was a good test!")
	}
	private def open4R2go(jmodel : JenaModel) : R2GoModel = {
		val r2goModel : R2GoModel = new rdf2go.impl.jena.ModelImplJena(jmodel)
		r2goModel.open
		r2goModel
	}
}
