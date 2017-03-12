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

package org.friendularity.infra.raiz

import java.lang.{Long => JLong}

import com.hp.hpl.jena
import jena.reasoner.{Reasoner, ReasonerRegistry}
import jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, InfModel}
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.impl.appro.ApproProfileGraphMaker

import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}

import org.cogchar.impl.legconf.{LegacyRepoFuncs, ChnkrWrapRepo}

import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.core.name.{ FreeIdent, Ident }
import org.appdapter.fancy.log.VarargsLogging;

import org.cogchar.blob.ghost.{GraphScanTest, IndexResult,  RRUtil, GHostUtil, GHostRecipeWrap}
import org.cogchar.blob.entry.{EntryHost, PlainEntry, FolderEntry, DiskEntryHost, ResourceEntryHost}

import org.cogchar.api.owrap.crcp.{BRFeature => CC_BRFeature}
import org.cogchar.api.owrap.appro.AFBRLegacyConfig

/*
class TestSetupLoader(val myRootNames : VizappTestRootNames) extends LegacyRepoFuncs with  VarargsLogging {

	def getMergedProfileGraph_RegularDesktop (profDataEntryHost : EntryHost) : JenaModel = {

		val activeTokens = Array[String]("all", "regular", "desktop")
		getMergedProfileGraph(activeTokens, profDataEntryHost)
	}
	def getMergedProfileGraph(activeTokens : Array[String], profDataEntryHost : EntryHost) : JenaModel = {

		val pgm = new ApproProfileGraphMaker(profDataEntryHost, myRootNames.pathToProfileFolder,  activeTokens)

		val mergedProfileGraph : JenaModel = pgm.makeMergedProfileGraph
		mergedProfileGraph
	}

	def  getUnitTestResourceEntryHost : EntryHost = {
		val dataMarkerClazz : java.lang.Class[_] = classOf[ApproRaizCtxImpl]
		val dataEntryHost : EntryHost = new ResourceEntryHost(dataMarkerClazz)
		dataEntryHost
	}

	def testLegConfLoad(profileJM : JenaModel, cdatEH : EntryHost) : Unit = {

		val cwRepoSpec = makeLegacyConfRepoSpec(profileJM, myRootNames.vzpLegCnfBrkrRcpUriTxt, cdatEH)

		val cwRepo = cwRepoSpec.getOrMakeRepo.asInstanceOf[ChnkrWrapRepo]

		val dirModel = cwRepo.getDirectoryModel()

		info1("testLegConfLoad: Fetched repo dir model: {}", dirModel)

		java.lang.Thread.sleep(3000)
		info0("That was a good test!")
	}

}
*/
object TestRaizLoad extends  VarargsLogging {
	/*
	lazy val dfltRootNames = new VizappTestRootNames {}
	lazy val dfltSetupLoader = new TestSetupLoader(dfltRootNames)
	def getDfltSetupLoader : TestSetupLoader = dfltSetupLoader
	*/
	def main(args: Array[String]): Unit = {

		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
		//	org.apache.log4j.BasicConfigurator.configure();
		//	org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		//
		//	Appears that currently Akka is automatically initing logging with our log4j.properties.

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestRaizLoad main().START");

		// GraphScanTest.setupScanTestLogging
		info0("Starting TestProfileLoad")

		/*
val setupLoader = getDfltSetupLoader

val profDataEntryHost: EntryHost = setupLoader.getUnitTestResourceEntryHost

val mergedProfileGraph = setupLoader.getMergedProfileGraph_RegularDesktop(profDataEntryHost)
*/

		val profileLoader = VizappProfileLoaderFactory.makeRaizUnitTestProfileLoader // makeVizappUnitTestProfileLoader
		val mergedProfileGraph = profileLoader.makeMergedProfileGraph
		info1("Fetched mergedProfileGraph of size {}", mergedProfileGraph.size: java.lang.Long)
		debug1("mergedProfileGraph dump:\n{}", mergedProfileGraph)

		val legacyLoader = VizappLegacyLoaderFactory.makeUnitTestLegacyLoader()
		val legacyELRC : EnhancedLocalRepoClient = legacyLoader.makeLegacyELRC(mergedProfileGraph)

		info1("Got legacy enhanced repo client: {}", legacyELRC)
		val repo = legacyELRC
//		val cwRepo = ???
//		val dirModel = cwRepo.getDirectoryModel()
//		info1("testLegConfLoad: Fetched repo dir model: {}", dirModel)

		java.lang.Thread.sleep(3000)
		info0("That was a good test!")

		/*
		val cdatEntryHost = profDataEntryHost // This could easily be a *different* eHost, instead!
		setupLoader.testLegConfLoad(mergedProfileGraph, cdatEntryHost)
		*/
	}
	def appendOntoAndInfer(inModel : JenaModel) : Unit = {
		val rdfsReasoner : Reasoner =  ReasonerRegistry.getRDFSReasoner
		val infModelWithRDFS : InfModel = JenaModelFactory.createInfModel(rdfsReasoner, inModel)
		val deductionsModel : JenaModel = infModelWithRDFS.getDeductionsModel

		info3("model sizes: inputWithOnto={}, deductions={}, inferred={}", inModel.size : JLong,
			deductionsModel.size : JLong,  infModelWithRDFS.size : JLong)

	}

}