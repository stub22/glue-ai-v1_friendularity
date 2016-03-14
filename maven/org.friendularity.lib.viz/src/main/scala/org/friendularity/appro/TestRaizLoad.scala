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


import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}


import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.core.name.{ FreeIdent, Ident }

import org.cogchar.blob.ghost.{GraphScanTest, IndexResult,  RRUtil, GHostUtil, GHostRecipeWrap}
import org.cogchar.blob.entry.{EntryHost, PlainEntry, FolderEntry, DiskEntryHost, ResourceEntryHost}


object TestRaizLoad extends VarargsLogging {
	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
		//	org.apache.log4j.BasicConfigurator.configure();
		//	org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		//
		//	Appears that currently Akka is automatically initing logging with our log4j.properties.

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestRaizLoad main().START");

		// http://onto.friendularity.org/indiv/vizappRecipes_reg_desk_2016Q1#vizapp_legConf_brokerRecipe

		GraphScanTest.setupScanTestLogging
		info0("Starting TestProfileLoad")

		val mergedProfileGraph = getMergedProfileGraph_RegularDesktop
		info1("Fetched mergedProfileGraph of size {}", mergedProfileGraph.size : java.lang.Long)
		debug1("mergedProfileGraph dump:\n{}", mergedProfileGraph)
	}
	def getMergedProfileGraph_RegularDesktop : JenaModel = {
		val activeTokens = Array[String]("all", "regular", "desktop")
		getMergedProfileGraph(activeTokens)
	}
	def getMergedProfileGraph(activeTokens : Array[String]) : JenaModel = {
		val profDataEntryHost : EntryHost = getProfileTestEntryHost

		// These two values are described in https://robokind.atlassian.net/browse/RFA-302
		// as MILO_PROFILE_FOLDER_PATH  and MILO_PROFILE_ACTIVE_TOKENS

		val pathToProfileFolder = "org/friendu/tchunk/vizapp_profile"

		val pgm = new ApproProfileGraphMaker(profDataEntryHost, pathToProfileFolder,  activeTokens)

		val mergedProfileGraph : JenaModel = pgm.makeMergedProfileGraph
		mergedProfileGraph
	}


	def  getProfileTestEntryHost : EntryHost = {
		val dataMarkerClazz : java.lang.Class[_] = classOf[ApproRaizCtxImpl]
		val dataEntryHost : EntryHost = new ResourceEntryHost(dataMarkerClazz)
		dataEntryHost
	}

}
