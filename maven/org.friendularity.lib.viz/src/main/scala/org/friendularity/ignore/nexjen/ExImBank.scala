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

package org.friendularity.ignore.nexjen

import java.io.ByteArrayOutputStream
import org.appdapter.core.matdat.OnlineSheetRepoSpec
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.core.store.Repo
import org.appdapter.demo.DemoResources
import org.appdapter.help.repo.{RepoClient, RepoClientImpl}
import org.appdapter.impl.store.FancyRepo

import org.cogchar.name.dir.{AssumedGraphDir, AssumedQueryDir}
import org.appdapter.demo.DemoResources

import org.appdapter.core.repo.{RepoSpec}
import org.appdapter.impl.store.{DatabaseRepoSpec, DatabaseRepoFactoryLoader}

import com.hp.hpl.jena.query.{ Dataset, DatasetFactory, ReadWrite }

import com.hp.hpl.jena.tdb.TDBFactory
import org.friendularity.respire.VarargsLogging

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * Your bank will lend you data
 */

object ExImBank extends VarargsLogging {
	val emptyFileResModelCLs = new java.util.ArrayList[ClassLoader]();
	def spec_legacyUnitTestData() : OnlineSheetRepoSpec = { 
		// GluePuma_HRKR50_TestFull
		val REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc" 
		val NAMESPACE_SHEET_NUM = 9
		val DIRECTORY_SHEET_NUM = 8
		new OnlineSheetRepoSpec(REPO_SHEET_KEY, NAMESPACE_SHEET_NUM, DIRECTORY_SHEET_NUM, emptyFileResModelCLs);
	}
	def spec_coreBehavTestData() : OnlineSheetRepoSpec = { 
		// GluePuma_HRKR25_CoreData_April2014
		// This works with ogwleb.25 to yield a functioning combined GUI for multi-browser interaction with v-char
		val REPO_SHEET_KEY = "0AsAJ7pzOB_F2dEI3V3hXY2tOUTNDazF5MWI4RFI2QlE" 
		val NAMESPACE_SHEET_NUM = 9
		val DIRECTORY_SHEET_NUM = 8
		new OnlineSheetRepoSpec(REPO_SHEET_KEY, NAMESPACE_SHEET_NUM, DIRECTORY_SHEET_NUM, emptyFileResModelCLs);
	}	
		// GluePuma_HRKR25_TestFull (default for opengl.r25)
		// 0AmvzRRq-Hhz7dFVpSDFaaHhMWmVPRFl4RllXSHVxb2c
		// 
		// https://docs.google.com/spreadsheet/ccc?key=0AsAJ7pzOB_F2dGVkcUlTSkJ1ZXhzUFVIckhxN25tQkE&usp=drive_web#gid=8
		// = GluePuma_HRKR25_CoreData_May2014_JustinsTestBuild
	
	def loadRepoClientFromSpec() {
	
	}
	def importRepoContentsInSingleXact(inputRepo : Repo.WithDirectory, tgtDataset : Dataset) { 
		
		if (!tgtDataset.supportsTransactions) {
			throw new RuntimeException("This method expects a transaction-aware target dataset")
		}
		val dfltGraphStatsJL : java.util.List[Repo.GraphStat] = inputRepo.getGraphStats();
		info2("Got {} input-repo graph stats: {}", dfltGraphStatsJL.size : java.lang.Integer, dfltGraphStatsJL)
	
		tgtDataset.begin(ReadWrite.WRITE);
		val inputMQDset = inputRepo.getMainQueryDataset
		inputMQDset.begin(ReadWrite.READ);
		try {
			import scala.collection.JavaConversions._

			val dfltGraphStats : List[Repo.GraphStat] = dfltGraphStatsJL.toList
			dfltGraphStats foreach (gs => {
					info1("Doing import for: {}", gs)
					// val tgtModelID = txRepo.makeIdentForURI(gs.graphURI)
					val graphURI = gs.graphURI 
					val tgtModelID =  inputRepo.makeIdentForURI(graphURI)
					val srcModel = inputRepo.getNamedModel(tgtModelID)
					info1("Fetched source model of size {} statements", srcModel.size : java.lang.Long)
					val tgtModel = tgtDataset.getNamedModel(graphURI)
					info1("PRE-load, fetched target model containing {} statements", tgtModel.size : java.lang.Long)
					tgtModel.add(srcModel)
					info1("POST-load, target model now contains {} statements", tgtModel.size : java.lang.Long)
				})
			tgtDataset.commit
		} catch {
			case t : Throwable => {
				getLogger.error("Caught error, rolling back", t)
				tgtDataset.abort()
			}
		} finally {
			// If we haven't committed or aborted, this will show:
			// 21571 [main] WARN TDB  - Transaction not commited or aborted: Transaction: 2 : Mode=WRITE : State=ACTIVE : --mem--/
			// ...and implicity abort.
			tgtDataset.end()
			inputMQDset.end();
		}
	}
	case class Stat(myName : String, myCount : Long) {
		
	}
	def ourStatsFromRepo (r : Repo.WithDirectory) : List[Stat] = {
		var stats : List[Stat] = Nil
		import scala.collection.JavaConversions._
		val dfltGraphStatsJL : java.util.List[Repo.GraphStat] = r.getGraphStats();

		val dfltGraphStats : List[Repo.GraphStat] = dfltGraphStatsJL.toList
		dfltGraphStats foreach (gs => {	
			val s = new Stat(gs.graphURI, gs.getStatementCount)
			stats = stats :+ s
		})
		stats
	}
	def dumpStatsFromReadTrans(dset : Dataset) : List[Stat] = {
		dset.begin(ReadWrite.READ)
		var numModels : Int = 0
		var stats : List[Stat] = Nil
		try {
			import scala.collection.JavaConversions._
			dset.listNames foreach (graphName => {
					
				val m = dset.getNamedModel(graphName)
				numModels = numModels + 1
				info3("Found {}-th target model at {} containing {} statements", numModels : java.lang.Integer, graphName, m.size : java.lang.Long)
				val stat = new Stat(graphName, m.size())
				stats = stats :+ stat
			})			
		} finally {
			dset.end()
		}
		info1("Found {} models in total", numModels : java.lang.Integer)
		stats
	}
	def main(args: Array[String]) : Unit = {
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);
		val legacyUnitSpec = spec_legacyUnitTestData();	
		val behavCoreSpec = spec_coreBehavTestData();
		
		val legacyRepo = spec_legacyUnitTestData.makeRepo();
		
		val sourceStats = ourStatsFromRepo(legacyRepo)
		info2("Got {} sourceStats: {}", sourceStats.length : java.lang.Integer, sourceStats)
		// val legacyRC = legacyUnitSpec.makeRepoClient(legacyRepo);	
	
		// val behavCoreRepo = spec_coreBehavTestData.makeRepo();
		
		val txRepoID =  new FreeIdent("urn:org.cogchar/crazy#txRepo_499");
		val txRepo = new TxRepo(txRepoID)
		
		val inMemDSet = TDBFactory.createDataset()
		info1("inMemoryDSet.supportsTransactions()= {}", inMemDSet.supportsTransactions : java.lang.Boolean)
		
		val preStats = dumpStatsFromReadTrans(inMemDSet)
		
		importRepoContentsInSingleXact(legacyRepo, inMemDSet)
		
		val postStats = dumpStatsFromReadTrans(inMemDSet)
		
		val postStatSet = postStats.toSet
		val sourceStatSet = sourceStats.toSet
		val diff1 = sourceStatSet.diff(postStatSet)
		info1("sourceStats.diff(postStats) = {} ", diff1)
		val diff2 = postStatSet.diff(sourceStatSet)
		info1("postStats.diff(sourceStats) = {} ", diff2)

		// processInputDataset(behavCoreSpec)
		
		// println("B-Repo dataset: " + dbRepo.getMainQueryDataset())
	}	
}
