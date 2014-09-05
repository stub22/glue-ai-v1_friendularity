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

import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.core.store.Repo
import org.appdapter.demo.DemoResources
import org.appdapter.fancy.rclient.{RepoClient, RepoClientImpl, GraphPortalRepoClient}
import org.appdapter.fancy.rspec.{RepoSpec, RepoSpecForDirectory, SdbSqlRepoSpec, OnlineSheetRepoSpec, URLRepoSpec, URLDirModelRepoSpec}
import org.appdapter.fancy.loader.{SdbSqlRepoFactoryLoader}
import org.appdapter.fancy.repo.FancyRepo
import org.appdapter.fancy.gportal.{GraphPortal, GraphSupplier, SuppliedGraphStat, GraphPortalFuncs, DelegatingPortal, LazyLocalDelegatingPortal, LazyRemoteDelegatingPortal}
import org.appdapter.demo.DemoResources

import com.hp.hpl.jena.query.{ Dataset, DatasetFactory, ReadWrite }
import com.hp.hpl.jena.tdb.TDBFactory

import org.cogchar.name.dir.{AssumedGraphDir, AssumedQueryDir}


/**
 * @author Stu B. <www.texpedient.com>
 * 
 * Your bank will lend you data, and accept your deposits of data.
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
	def spec_testData_majL5SG() : OnlineSheetRepoSpec = { 
		// !!!!!!!!!!!Copy of  
		// Lesson_5_SCENEGEN_GluePuma_HRKR25_CoreData_May2014_JustinsTestBuild_SCENEGEN_Lesson_5
		val REPO_SHEET_KEY = "0AivIV8RvlFTvdFpmQkYzclZUNHdLUHN2RFRkUS1scEE" 
		val NAMESPACE_SHEET_NUM = 9
		val DIRECTORY_SHEET_NUM = 8
		new OnlineSheetRepoSpec(REPO_SHEET_KEY, NAMESPACE_SHEET_NUM, DIRECTORY_SHEET_NUM, emptyFileResModelCLs);
	}
	def specForFolderWithDir(dirModelURL : String) : RepoSpec = { 
		val fileModelCLs: java.util.List[ClassLoader] = emptyFileResModelCLs
		// Both of these forms work.
		// val spec =	new URLRepoSpec(dirModelURL, fileModelCLs) 
		val spec = new URLDirModelRepoSpec(dirModelURL, fileModelCLs)
		spec
	}
	//  puma.boot.config.local="GlueSystemData/dir.ttl" 
	// file:/E:/_mount/hrk_closedsrc_trunk/maven/com.rkbots.bundle.oglweb.R25/GlueSystemData/dir.ttl
	val rkClosedMaven = "file:/E:/_mount/hrk_closedsrc_trunk/maven/"
	val oglwebR25 = rkClosedMaven + "com.rkbots.bundle.oglweb.R25/RK_R25_Demo_OnDisk/dir.ttl"
	val oglwebGlue = rkClosedMaven + "com.rkbots.bundle.oglweb.R25/GlueSystemData/dir.ttl"
	val headlessR25 : String = rkClosedMaven + "com.rkbots.headless.r25web/RK_R25_Demo_OnDisk/dir.ttl"
	def spec_headlessR25() : RepoSpec = 	specForFolderWithDir(headlessR25)
	def spec_oglwebGlue() : RepoSpec = specForFolderWithDir(oglwebGlue) 
	def spec_oglwebR25() : RepoSpec = specForFolderWithDir(oglwebR25) 

	/*	// GluePuma_HRKR25_TestFull (default for opengl.r25)
	 // 0AmvzRRq-Hhz7dFVpSDFaaHhMWmVPRFl4RllXSHVxb2c
	 // https://docs.google.com/spreadsheet/ccc?key=0AsAJ7pzOB_F2dGVkcUlTSkJ1ZXhzUFVIckhxN25tQkE&usp=drive_web#gid=8
	 // = GluePuma_HRKR25_CoreData_May2014_JustinsTestBuild
	 As of July 23, Major is testing on R-25 DevBoard + Robot using:
	 !!!!!!!!!!!Copy of  
	 Lesson_5_SCENEGEN_GluePuma_HRKR25_CoreData_May2014_JustinsTestBuild_SCENEGEN_Lesson_5
	 https://docs.google.com/spreadsheet/ccc?key=0AivIV8RvlFTvdFpmQkYzclZUNHdLUHN2RFRkUS1scEE&usp=sharing
	 !!!!!!!!!!!!!!!!!Copy of 
	 Lesson_5_SCENEGEN_GluePuma_HRKR25_BehavData_May2014_JustinsTestBuild_SCENEGEN_Lesson_5
	 https://docs.google.com/spreadsheet/ccc?key=0AivIV8RvlFTvdEFRaktNYmRjNjFYT0dRTnR0NVgtVkE&usp=sharing
	 */
	/*				
	 val tgtModel = tgtDataset.getNamedModel(graphURI)
	 info1("PRE-load, fetched target model containing {} statements", tgtModel.size : java.lang.Long)
	 tgtModel.add(srcModel)
	 info1("POST-load, target model now contains {} statements", tgtModel.size : java.lang.Long)
	 */
	class AnalyzedGraphSpace4(val mySupplier : GraphSupplier) {
		lazy val myBasicStats = mySupplier.fetchStats(None)
		lazy val myFactoredSnaps = AnalyzedGraphSpace4.makeFactoredSnaps(mySupplier)
		lazy val myIntraCompared = AnalyzedGraphSpace4.intraCompareSnaps(myFactoredSnaps)		
	}
	object AnalyzedGraphSpace4 {
		def makeFactoredSnaps(supplier : GraphSupplier) : List[FactoredGraphSnap] = { 
			var results : List[FactoredGraphSnap] = Nil
			val sourceStats = supplier.fetchStats(None)
			for (s <- sourceStats) {
				val sourceModel = supplier.getNamedGraph_Readonly(s.myAbsUriTxt)
				val factoredSnap = new FactoredGraphSnap(sourceModel, s)
				results ::= factoredSnap 
			}
			results.reverse
		}
		def intraCompareSnaps(fgsnaps : Iterable[FactoredGraphSnap]) : List[FactoredComparison] = {
			var unsortedComparisons : List[FactoredComparison] = Nil
			val allIn = fgsnaps.toList
			val comboPairsIterator : Iterator[List[FactoredGraphSnap]] =  allIn.combinations(2)
			for (comboPairList : List[FactoredGraphSnap] <- comboPairsIterator) {
				if (comboPairList.size != 2) throw new Exception("ComboPairList is not of size 2:[" + comboPairList + "]")
				unsortedComparisons ::= new FactoredComparison(comboPairList.head, comboPairList.last) 
			}
			unsortedComparisons.sortBy(_.myAvgJaccsim)
		}
	}			
	def main(args: Array[String]) : Unit = {
		org.appdapter.bind.log4j.Log4jFuncs.forceLog4jConfig(new java.net.URL("file:src/main/resources/log4j.properties"));
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		//org.apache.log4j.BasicConfigurator.configure();
		//org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);
		//	val legacyUnitSpec = spec_legacyUnitTestData();	
		//	val behavCoreSpec = spec_coreBehavTestData();
		val sheetSpec_L5SG =  spec_testData_majL5SG();
		val folderSpec_headless = spec_headlessR25(); 
		val folderSpec_oglwebGlue = spec_oglwebGlue();
		val folderSpec_oglwebR25 = spec_oglwebR25();

		val anzd_headlessR25 = loadAndAnalyze(folderSpec_headless)
		printStuff(anzd_headlessR25)

		val anzd_oglwebGlue = loadAndAnalyze(folderSpec_oglwebGlue)
		printStuff(anzd_oglwebGlue)

		val anzd_oglwebR25 = loadAndAnalyze(folderSpec_oglwebR25)
		printStuff(anzd_oglwebR25)	
		
		val anzd_L5SG = loadAndAnalyze(sheetSpec_L5SG)
		printStuff(anzd_L5SG)
		
		/*
		val legacySpec = folderSpec_headless 
		// val legacySpec = testSpec_L5SG
		val legacyRepo = legacySpec.getOrMakeRepo(); //  spec_legacyUnitTestData.makeRepo();
		val legacyDset = legacyRepo.getMainQueryDataset
		val legacyPortal = new LazyLocalDelegatingPortal(legacyDset)
		val legacyResGraphID = new FreeIdent("http://onto.friendularity.org/names#dirGraphURI");
		val dftQueryTgtVarName = ""
		val dQrySrcGrphQName = ""
		val legacyPortalRC = new GraphPortalRepoClient(legacyPortal, legacyResGraphID, dftQueryTgtVarName, dQrySrcGrphQName)
		val legacySupplier = legacyPortal.getSupplier

		val ags = new AnalyzedGraphSpace4(legacySupplier)
		*/
		// val legacyRC = legacyUnitSpec.makeRepoClient(legacyRepo);	
	
		// val behavCoreRepo = spec_coreBehavTestData.makeRepo();
		
		//	importFromLegacyPortal(legacyPortal)
	}
	def printStuff(ags : AnalyzedGraphSpace4) {
		info2("Got {} sourceStats: {}", ags.myBasicStats.length : java.lang.Integer, ags.myBasicStats)	
		info2("Got {} factoredSnaps: {}", ags.myFactoredSnaps.length : java.lang.Integer, ags.myFactoredSnaps)
		info2("Got {} comparisons: {}", ags.myIntraCompared.size : java.lang.Integer, ags.myIntraCompared)		
	}
	def loadLegacyLocalPortal (legacySpec : RepoSpec) : DelegatingPortal = {
		val legacyRepo = legacySpec.getOrMakeRepo(); 
		val legacyDset = legacyRepo.getMainQueryDataset
		val legacyPortal = new LazyLocalDelegatingPortal(legacyDset)
		legacyPortal
	}
	def loadAndAnalyze(rspec : RepoSpec) : AnalyzedGraphSpace4 = {
		val legacySpec = rspec 
		val legacyPortal = loadLegacyLocalPortal(legacySpec)
		val legacyResGraphID = new FreeIdent("http://onto.friendularity.org/names#dirGraphURI");
		val dftQueryTgtVarName = ""
		val dQrySrcGrphQName = ""
		val legacyPortalRC = new GraphPortalRepoClient(legacyPortal, legacyResGraphID, dftQueryTgtVarName, dQrySrcGrphQName)
		val legacySupplier = legacyPortal.getSupplier

		val ags = new AnalyzedGraphSpace4(legacySupplier)
		ags
	}
	def importFromLegacyPortal(legacyPortal: DelegatingPortal) {
		val txRepoID =  new FreeIdent("urn:org.cogchar/crazy#txRepo_499");
		val txRepo = new TxRepo(txRepoID)
		
		val inMemDSet = TDBFactory.createDataset()
		info1("inMemoryDSet.supportsTransactions()= {}", inMemDSet.supportsTransactions : java.lang.Boolean)
		
		val targetLocalPortal = new LazyLocalDelegatingPortal(inMemDSet)
		
		val flag_overwriteTgtGraphs : Boolean = true

		GraphPortalFuncs.copyGraphsAndShowStats(legacyPortal, targetLocalPortal, flag_overwriteTgtGraphs)
		/*
		 http://host/dataset/query -- the SPARQL query endpoint.
		 http://host/dataset/update -- the SPARQL Update language endpoint.
		 http://host/dataset/data -- the SPARQL Graph Store Protocol endpoint.
		 http://host/dataset/upload -- the file upload endpoint.
		 */		
		
		val remotePortalBaseURL = "http://lima.nodeset.com:4001/temp_major_L5SG_4001";
		val remoteGraphStoreURL = remotePortalBaseURL + "/data"
		val remoteQueryURL = remotePortalBaseURL + "/query"
		val remoteUpdateURL = remotePortalBaseURL + "/update"
		val remoteUploadURL = remotePortalBaseURL + "/upload"
		
		val tgtRemotePortal = new LazyRemoteDelegatingPortal(remoteGraphStoreURL, remoteQueryURL, remoteUpdateURL)
		
		// GraphPortalFuncs.copyGraphsAndShowStats(legacyPortal, tgtRemotePortal, flag_overwriteTgtGraphs)
		
		val targetGS : GraphSupplier = targetLocalPortal.getSupplier
		val tgtStatsAfter : List[SuppliedGraphStat] = targetGS.fetchStats(None)
		tgtStatsAfter.foreach( gsA => {
				tgtStatsAfter.foreach( gsB => {
						val cr = compareGraphs(targetGS, gsA.myAbsUriTxt, targetGS, gsB.myAbsUriTxt)
						info1("Result: {}", cr)
					})
			})
		

		// val remoteBank = new RemoteSohBank(remoteBankURL)
		// processInputDataset(behavCoreSpec)
		
		// println("B-Repo dataset: " + dbRepo.getMainQueryDataset())
	}	
	def compareGraphs(supplier_A : GraphSupplier, graphAbsUri_A : String, 
					  supplier_B : GraphSupplier, graphAbsUri_B : String) : GraphComparisonResult  = {
	
		val result = new GraphComparisonResult
		result.mySupplier_A = supplier_A;
		result.myGraphAbsUri_A = graphAbsUri_A;
		result.mySupplier_B = supplier_B
		result.myGraphAbsUri_B = graphAbsUri_B
		val model_A = supplier_A.getNamedGraph_Readonly(graphAbsUri_A)
		result.myStmtCount_A = model_A.size
		val model_B = supplier_B.getNamedGraph_Readonly(graphAbsUri_B)
		result.myStmtCount_B = model_B.size
		result;
	}
}
import com.hp.hpl.jena.rdf.model.{Model, ModelFactory, Resource, Property, Literal, RDFNode, Statement}
class FactoredGraphSnap(srcModel : Model, val mySrcStat : SuppliedGraphStat) {
	import scala.collection.JavaConverters._
	// We typically get empty prefix maps on models fetched from TDB
	val myPrefixMap : Map[String,String] = srcModel.getNsPrefixMap.asScala.toMap
// 	val myPrefixMap : Map[String,String] = mySnappedModel.getNsPrefixMap.asScala.toMap

	val mySnappedModel = ModelFactory.createDefaultModel()
	mySnappedModel.add(srcModel)
	var mySubjects : Set[Resource] = Set[Resource]()
	var myProperties : Set[Property] = Set[Property]()
	var myObjResrcs : Set[Resource] = Set[Resource]()
	var myObjLits : Set[Literal] = Set[Literal]()

	// myPrefixMap ++= mySnappedModel.getNsPrefixMap.asScala

	val snappedStmtIt = mySnappedModel.listStatements()
	while (snappedStmtIt.hasNext) {
		val stmt : Statement = snappedStmtIt.nextStatement
		mySubjects += stmt.getSubject
		myProperties += stmt.getPredicate
		val objNode : RDFNode = stmt.getObject
		if (objNode.isResource) {
			myObjResrcs += objNode.asResource
		} else if (objNode.isLiteral) {
			myObjLits += objNode.asLiteral
		} else {
			throw new Exception("Statement-object Node is not a literal or a resource:" + objNode)
		}
	}
	
	override def toString () : String = {
		"FactoredGraphSnap[srcStat={" + mySrcStat + "}, prefixMap=" + myPrefixMap + ", props=" + myProperties + 
		", subjectCount=" + mySubjects.size + ", objResCount=" + myObjResrcs.size + ", objLitCount=" +
		myObjLits.size + "]\n"
	}
}
class SetComparison[X](val mySetA : Set[X], val mySetB : Set[X]) {
	lazy val myIntersect = mySetA.intersect(mySetB)
	lazy val myUnion = mySetA.union(mySetB)
	lazy val mySymDiff = myUnion.diff(myIntersect)
	lazy val mySpecialA = mySetA.diff(mySetB)
	lazy val mySpecialB = mySetB.diff(mySetA)
	// 0 to 1      http://en.wikipedia.org/wiki/Jaccard_index
	lazy val myJaccardSimilarity : Float = {
		val unionSize : Int = myUnion.size
		if (unionSize == 0) 1.0f else myIntersect.size.toFloat / unionSize.toFloat
	} 
}
class FactoredComparison(val mySnap_A : FactoredGraphSnap, val mySnap_B : FactoredGraphSnap) {
	lazy val myPropComparison = new SetComparison[Property](mySnap_A.myProperties, mySnap_B.myProperties)
	lazy val mySubjComparison = new SetComparison[Resource](mySnap_A.mySubjects, mySnap_B.mySubjects)
	lazy val myObjResComparison = new SetComparison[Resource](mySnap_A.myObjResrcs, mySnap_B.myObjResrcs)
	lazy val myObjLitComparison = new SetComparison[Literal](mySnap_A.myObjLits, mySnap_B.myObjLits)

	lazy val myPropJaccsim : Float = myPropComparison.myJaccardSimilarity
	lazy val mySubjJaccsim : Float = mySubjComparison.myJaccardSimilarity 
	lazy val myObjResJaccsim : Float = myObjResComparison.myJaccardSimilarity 
	lazy val myObjLitJaccsim : Float = myObjLitComparison.myJaccardSimilarity
		
	lazy val myStmtUnion = mySnap_A.mySnappedModel.union(mySnap_B.mySnappedModel)
	lazy val myStmtIntersect = mySnap_A.mySnappedModel.intersection(mySnap_B.mySnappedModel)
	lazy val myStmtSpecialA = mySnap_A.mySnappedModel.difference(mySnap_B.mySnappedModel)
	lazy val myStmtSpecialB = mySnap_B.mySnappedModel.difference(mySnap_A.mySnappedModel)

	lazy val myStmtJaccsim : Float = {
		val unionSize : Long = myStmtUnion.size 
		if (unionSize == 0) 1.0f else myStmtIntersect.size.toFloat / unionSize.toFloat
	}
	
	lazy val myAvgJaccsim = (myStmtJaccsim + myPropJaccsim + mySubjJaccsim + myObjResJaccsim + myObjLitJaccsim) / 5.0

	override def toString () : String = {
		"FactoredComparison[avgJaccSim=" + myAvgJaccsim + ", stmtJS=" + myStmtJaccsim + 
		", propJS=" + myPropJaccsim + ", subjJS=" + mySubjJaccsim +
		", objResJS=" + myObjResJaccsim + ", objLitJS=" + myObjLitJaccsim +  
		", snapA=" + mySnap_A + ", snapB=" + mySnap_B + "]\n\n\n"
	}
}

case class GraphComparisonResult {
	var mySupplier_A : GraphSupplier = null
	var mySupplier_B : GraphSupplier = null
	var	myGraphAbsUri_A : String = null 
	var	myGraphAbsUri_B : String = null
	var myStmtCount_A : Long = -1
	var myStmtCount_B : Long = -1
	
	override def toString() : String = {
		"A=[" + mySupplier_A + ", " + myGraphAbsUri_A + ", " + myStmtCount_A + ", B=[" + mySupplier_B + ", " + myGraphAbsUri_B + ", " + myStmtCount_B + "]"
	}
}


/*
 * http://lima.nodeset.com:4001/raw/cogchar_res_core/onto/BehavChanAct_owl2.ttl
 * 
 RDF files accessed via URL in FROM clause works with Fuseki's general SPARQLer form, but not with 
 the dataset-pre-focused-default  query form, which is interpreting it as a graph URI which may either identify a 
 subset of default set, or potentially  identify a graph resolvable via OntoDocMgr in a way similar to owl:imports.

 PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 SELECT  *
 FROM    <http://lima.nodeset.com:4001/raw/cogchar_res_core/onto/BehavChanAct_owl2.ttl>
 WHERE   { ?s ?p ?o }

 */
/* Want to match-strip redundant folder detail to fit with legacy generated file paths like:
 In:    .../GlueSystemData/dir.ttl   which contains a fileRepo for the rest of the files described as this:
 csi:filerepo_2014041611_1430_490          rdf:type ccrt:FileRepo ;
 ccrt:sourcePath "GlueSystemData/" .
	  
 ... which appears to succeed now only when relative to the current working directory of the loading process.
 Is it currently checking bundle-paths supplied in the fileModelCLs?
 Are we ready to move forward with Jena 2.10+ RDFDataMgr?
	  
 */ 