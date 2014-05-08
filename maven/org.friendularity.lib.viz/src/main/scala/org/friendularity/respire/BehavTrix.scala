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

package org.friendularity.respire

/**
 * @author Stu B. <www.texpedient.com>
 */
import org.appdapter.core.matdat.URLRepoSpec;
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }
import org.appdapter.help.repo.{RepoClient, RepoClientImpl, InitialBindingImpl} 
import org.appdapter.impl.store.{FancyRepo};
import org.appdapter.core.matdat.{SheetRepo, OnlineSheetRepoSpec}
import com.hp.hpl.jena.query.{QuerySolution} // Query, QueryFactory, QueryExecution, QueryExecutionFactory, , QuerySolutionMap, Syntax};
import com.hp.hpl.jena.rdf.model.{Model}
import org.appdapter.core.log.BasicDebugger;

import org.appdapter.impl.store.{ModelClientImpl, ResourceResolver};

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}


object BehavTrix extends VarargsLogging {

	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		// loadBonusRepo();
		
		val j = new Jiggy()
		j.go()
	}
	val winDirPath = "E:\\_mount\\_supply\\rk_char_repo_snaps\\rk_bude_lessons\\LessonDemo_20140506\\LessonDemo\\dir.ttl"

	def loadBonusRepo() {
		//  new File(tempfile.getAbsolutePath()).toURI().toURL()
		// "file:/home/matt/dev/projects/exports/LessonDemo/dir.ttl"
		
		val winFile = new java.io.File(winDirPath)
		info2("winFile={} canRead={}", winFile, winFile.canRead : java.lang.Boolean)
		val wfURI = winFile.toURI()  
		val wfURL = wfURI.toURL
		info2("URI={}   URL={}", wfURI, wfURL)
		val wfutxt = wfURL.toString
		val ursPath = wfutxt; //  = "file:///E:/_mount/_supply/rk_char_repo_snaps/rk_bude_lessons/LessonDemo_20140506/LessonDemo/dir.ttl"
		info1("****************************** loading from URL path:\n{}", ursPath)
		
		val uRepSpec = new URLRepoSpec(ursPath);
		
	}
}

import com.hp.hpl.jena.query.{ Dataset, DatasetFactory }

import com.hp.hpl.jena.tdb.TDBFactory
import com.hp.hpl.jena.query.ReadWrite
import com.hp.hpl.jena.sparql.util.Context

import org.apache.jena.riot.RDFDataMgr

class Jiggy extends VarargsLogging {
	val pmod_01_uritxt = "uri:model_K301"
	val tdb_A_dirtxt = "jig_tdb_A416"
	def go() {
		// public static Dataset assembleDataset(String assemblerFile
		// public static Dataset createDataset(String dir)
		val inMemDSet = TDBFactory.createDataset()
		val fsysDSet = TDBFactory.createDataset(tdb_A_dirtxt);
		
		val mc = inMemDSet.getContext
		val fc = fsysDSet.getContext
		
		info2("mc={}      fc={}", mc, fc)
		info2("mc.keys={}      fc.keys={}", mc.keys, fc.keys)
		
		val winDirFile = new java.io.File(BehavTrix.winDirPath)
		val wdfURI = winDirFile.toURI() 
		info1("wdfURI={}", wdfURI)
		val srcDirModel = RDFDataMgr.loadModel(wdfURI.toString)
		info1("srcDirModel={}", srcDirModel) 
		
		fsysDSet.begin(ReadWrite.WRITE);
		try {
			// void	addNamedModel(String uri, Model model)
			// boolean containsNamedModel(String uri)
			// getContext()
			// getDefaultModel() / setDefaultModel()
			// Iterator<String>	listNames()
			// replaceNamedModel(String uri, Model model)
			// removeNamedModel(String uri)
			// Model	getNamedModel(String uri)
			val m1 = fsysDSet.getNamedModel(pmod_01_uritxt)
			val teller = new TellMe
			m1.register(teller)
			m1.add(srcDirModel)
			info0("\n\n\n\n***************** Adding again")
			m1.add(srcDirModel)
			info0("\n\n\n\n***************** Committing")
			fsysDSet.commit() ;
			
		} finally { 
			fsysDSet.end() ; 
			// Close the dataset, potentially releasing any associated resources. 
			// The dataset can not be used for query after this call.
			fsysDSet.close(); 
			inMemDSet.close();
		}
		
	}
}
import com.hp.hpl.jena.rdf.model.Statement
import com.hp.hpl.jena.rdf.listeners.StatementListener

class TellMe extends StatementListener {
	val myLog = new VarargsLogging
	
	var myAddedCount = 0
	var myRemovedCount = 0
	
	override def addedStatement(s : Statement) {
		myAddedCount += 1
		myLog.info2("addedStmt count={}, stmt={}", myAddedCount : Integer, s)
	}
	override def removedStatement(s : Statement) {
		myRemovedCount += 1
		myLog.info2("removedStmt count={}, stmt={}", myRemovedCount : Integer, s)
	}
}
/*
 *
Example 2 : Using the RDFDataMgr
In versions of Jena priot to 2.10.0, the FileManager provided some of this functionality. 
It was more basic, and not properly web enabled. The RDFDataMgr superceeds the FileManager. 
"load*" operations create an in-memory container (model, or dataset as appropriate); 
"read" operations add data into an existing model or dataset.

// Create a model and read into it from file 
// "data.ttl" assumed to be Turtle.
Model model = RDFDataMgr.loadModel("data.ttl") ;

// Create a dataset and read into it from file 
// "data.trig" assumed to be TriG.
Dataset dataset = RDFDataMgr.loadDataset("data.trig") ;

// Read into an existing Model
RDFDataMgr.read(model, "data2.ttl") ;
 * 
Multi-threaded use
Each dataset object has one transaction active at a time. The usual idiom within multi-threaded applications is 
to have one dataset per thread, and so there is one transaction per thread.
Each thread has a separate dataset object; these safely share the same storage but have independent transactions.
While it is possible to share a transaction between multiple threads, this is not encouraged. 

Multi JVM
Multiple applications, running in multiple JVMs, using the same file databases is not supported. There must be a 
single JVM controlling the database directory and files.

Use Fuseki to provide a database server for multiple applications. Fuseki supports SPARQL Query, SPARQL Update 
and the SPARQL Graph Store protocol.
 
 */

import org.appdapter.core.store.{ Repo, BasicRepoImpl, BasicStoredMutableRepoImpl, QueryProcessor, InitialBinding, ModelClient }

class ExplicitDatasetRepo(val myDset: Dataset, val myDirGraphID: Ident) extends BasicRepoImpl with FancyRepo with Repo.Updatable {
 // def loadSheetModelsIntoMainDataset(): Unit = {}
//  def loadDerivedModelsIntoMainDataset(fileModelCLs: java.util.List[ClassLoader]): Unit = {}
//  def loadFileModelsIntoMainDataset(fileModelCLs: java.util.List[ClassLoader]): Unit = {}
//  val myDirectoryModel: Model = directoryModel;

	override def getDirectoryModel: Model = getNamedModel(myDirGraphID);
  
	override def callLoadingInLock() {
		throw new RuntimeException("The callLoadingInLock method was called")
	}

}
// class DatabaseRepo(store: Store, val myDirGraphID: Ident)
//   extends BasicStoredMutableRepoImpl(store) with FancyRepo with Repo.Mutable with Repo.Stored {

		/*
		 * 
That xlsx file started out as an offline version of the content, but then
as it was edited over time it was eventually pointed back at the
googledocs. So only the dir tab of the xlsx is being used. That dir tab is
pointing to 4 different googledocs to load the actual content.

I just used the repo export to create a set of local ttl files using the
command: "java -jar com.rkbots.tools.repoexport
GluePuma_HRKR25_TestFull_OnDisk.xlsx LessonDemo/"
The ttl files are checked in at:
https://robokind.unfuddle.com/svn/robokind_rkcontent/trunk/content/metadata/LessonDemo

You can copy the LessonDemo folder to the top-level folder of the snapshot
(so you have LessonDemo at the same level as
GluePuma_HRKR25_TestFull_OnDisk.xlsx).
I've attached a new Start_HRK_Avatar.bat file pointing to
LessonDemo/dir.ttl (Replaced GluePuma_HRKR25_TestFull_OnDisk.xlsx with
LessonDemo/dir.ttl)
Run the new bat file and it will load the ttl files.

		 */
		// val r = uRepSpec.makeRepo();
		/* Eventually does this,
		import com.hp.hpl.jena.util.FileManager;
        val jenaFileMgr = JenaFileManagerUtils.getDefaultJenaFM
        JenaFileManagerUtils.ensureClassLoadersRegisteredWithJenaFM(jenaFileMgr, clList)
        jenaFileMgr.loadModel(rdfURL);
		which, outside OSGi, gets:  
223 [main] ERROR org.appdapter.bind.rdf.jena.model.JenaFileManagerUtils  - Mismatched Jena FMs: com.hp.hpl.jena.util.FileManager@6c4fc156!=com.hp.hpl.jena.util.FileManager@681e2ca7
		
224 [main] ERROR org.appdapter.core.matdat.FancyRepoLoader$  - Caught error loading file [file:/E:/_mount/_supply/rk_char_repo_snaps/rk_bude_lessons/LessonDemo_20140506/LessonDemo/dir.ttl, java.lang.NullPointerException]
Exception in thread "main" java.lang.NullPointerException
	at org.appdapter.bind.rdf.jena.model.JenaFileManagerUtils.ensureClassLoadersRegisteredWithJenaFM(JenaFileManagerUtils.java:65)
	at org.appdapter.core.matdat.FancyRepoLoader$.readModelSheetFromURL(RepoLoader.scala:131)
	at org.appdapter.core.matdat.FancyRepoLoader$.readDirectoryModelFromURL(RepoLoader.scala:109)
	at org.appdapter.core.matdat.URLDirModelRepoSpec.getDirectoryModel(FileModelRepo.scala:96)
	at org.appdapter.core.matdat.RepoSpecForDirectory.makeRepo(RepoSpec.scala:62)		 
		*/
		
		/*
E:\_mount\_supply\rk_char_repo_snaps\rk_bude_lessons\LessonDemo_20140506\LessonDemo\dir.ttl
cd "qpid-broker-0.12\bin"
start qpid-server.bat
PING 1.1.1.1 -n 1 -w 5000 >NUL
cd "..\..\sapi_speech_20120702_010_02"
start SAPIServer.exe
PING 1.1.1.1 -n 1 -w 1000 >NUL
cd ".."
java -Dcom.hrkind.demo.behavior.master.source="sheetKey;0AmvzRRq-Hhz7dFVpSDFaaHhMWmVPRFl4RllXSHVxb2c,
namespaceSheetNumber;9,directorySheetNumber;8, 
workBookPath;GluePuma_HRKR25_TestFull_OnDisk.xlsx, 
workBookNamespaceSheet;Nspc.csv, workBookDirectorySheet;Dir.csv" 
-Dcom.hrkind.robot.connections="robot01; 127.0.0.1, avatar01; 127.0.0.1" 
-Dpuma.boot.config.local="LessonDemo/dir.ttl" 
-Dbehavior.master.type=swing -jar felix.jar
*/		


