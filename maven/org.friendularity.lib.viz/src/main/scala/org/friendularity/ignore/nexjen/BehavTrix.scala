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

/**
 * @author Stu B. <www.texpedient.com>
 */
import org.appdapter.core.repo._
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }
import org.appdapter.help.repo.{RepoClient, RepoClientImpl, InitialBindingImpl} 
import org.appdapter.impl.store._
import org.appdapter.core.repo._
import com.hp.hpl.jena.query.{QuerySolution} // Query, QueryFactory, QueryExecution, QueryExecutionFactory, , QuerySolutionMap, Syntax};
import com.hp.hpl.jena.rdf.model.{Model}
import org.appdapter.core.log.BasicDebugger

import org.appdapter.impl.store.{ModelClientImpl, ResourceResolver};

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}

/*http://jena.apache.org/documentation/tdb/tdb_transactions.html
 * "Transactional TDB works with SPARQL Query, SPARQL Update, SPARQL Graph Store Update as well as the full Jena API."
 */

abstract sealed class ModernDatasetSpec {
}
case class MemoryDatasetSpec extends ModernDatasetSpec
case class FsdirDatasetSpec extends ModernDatasetSpec
case class AssemblerGraphDatasetSpec extends ModernDatasetSpec { // An assembler graph in some form
}

import org.friendularity.respire.VarargsLogging

object BehavTrix extends VarargsLogging {

	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);
		
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

			// Our goal in fabric layering is to provide for exec of algos like the fluent calculus solver.
			// The metadata we are processing thru is governing the flow of timeseries data through filters.
			// 
			// graph-level eventing can probably be based on transaction emit


class Jiggy extends VarargsLogging {
	val pmod_01_uritxt = "uri:model_K301"
	val tdb_A_dirtxt = "jig_tdb_A416"
	def go() {
		
		import org.appdapter.gui.demo.DemoBrowser;
		DemoBrowser.ensureRunning(true, "no-args");
		DemoBrowser.show();
		
		// public static Dataset assembleDataset(String assemblerFile
		// public static Dataset createDataset(String dir)
		val inMemDSet = TDBFactory.createDataset()
		val fsysDSet = TDBFactory.createDataset(tdb_A_dirtxt);
	
		DemoBrowser.showObject("fsysDSet", fsysDSet, false, false); 
		DemoBrowser.showObject("inMemDSet", inMemDSet, false, false); 

		val mc = inMemDSet.getContext
		val fc = fsysDSet.getContext
	
		info2("inMemDset.class={}, fsusDSet.class={}", inMemDSet, fsysDSet)
		info2("mc={}      fc={}", mc, fc)
		info2("mc.keys={}      fc.keys={}", mc.keys, fc.keys)
		
		val winDirFile = new java.io.File(BehavTrix.winDirPath)
		val wdfURI = winDirFile.toURI() 
		info1("wdfURI={}", wdfURI)
		
		val monitoredFSDSet = makeMonitoredDataset(fsysDSet)
		   
		//val srcDirModel = RDFDataMgr.loadModel(wdfURI.toString)
		//info1("srcDirModel={}", srcDirModel) 
		
	//	monitoredFSDSet.begin(ReadWrite.WRITE);
		try {
			// void	addNamedModel(String uri, Model model)
			// boolean containsNamedModel(String uri)
			// getContext()
			// getDefaultModel() / setDefaultModel()
			// Iterator<String>	listNames()
			// replaceNamedModel(String uri, Model model)
			// removeNamedModel(String uri)
			// Model	getNamedModel(String uri)
			val m1 = monitoredFSDSet.getNamedModel(pmod_01_uritxt)
			val teller = new TellMe
			// Teller hears about API level calls to modify the model, not the underlying real graph changes.
			// Question:  can the teller listen to effects of SPARQL-update?  
			// Seems like maybe no, because that's applied at the graph level rather than the model level.
			// What we really want is to listen to changes in the graphStore.
			// UPDATE:  Experimental config below confirms that SPARQL-update changes are not reported to
			// ModelChangeEvent listener, whereas LOAD commands are in fact reported.
			m1.register(teller)

			val loadingGraphName_A = "http://loadex-07-A/gAg";	
			val mLoadingA = monitoredFSDSet.getNamedModel(loadingGraphName_A)
			mLoadingA.register(teller)
			
			val loadingGraphName_B = "http://loadex-07-B/gBg";			
			val mLoadingB = monitoredFSDSet.getNamedModel(loadingGraphName_B)
			mLoadingB.register(teller)

			loadFromFile(monitoredFSDSet, wdfURI.toString, loadingGraphName_A)
			
			val sutxt = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ex: <http://example.org/>
			
INSERT DATA {  GRAPH <""" + loadingGraphName_B + """> {
	ex:subject1 ex:title "foozzly" ;
	ex:description "bardtleuix" ;
	rdf:type ex:FooBar .  
}}
			
"""
			
			info1("Before update, model-B contents={}", mLoadingB)
			
			applyUpdateText(monitoredFSDSet, sutxt)
			
			info1("After update, but before commit, model-B contents={}", mLoadingB)
			
			mLoadingB.add(mLoadingA)
			
			// m1.add(srcDirModel)
			// info0("\n\n\n\n***************** " + 
			// "Adding same triples again, so no actual change is happening in the graph, but we get another set of callbacks to TellMe ")				  
			// m1.add(srcDirModel)
			info0("\n\n\n\n***************** Committing")
			monitoredFSDSet.commit() ;
						
		} finally { 
			info0("\n\n\n\n***************** Ending")
			// Exception in thread "main" java.lang.UnsupportedOperationException: Transactions not supported
			monitoredFSDSet.end(); 
			// Close the dataset, potentially releasing any associated resources. 
			// The dataset can not be used for query after this call.
			// Not important in terms of TDB/ARQ impact, in the transactional case: 
			// http://mail-archives.apache.org/mod_mbox/jena-users/201310.mbox/%3C524AE601.3050901@apache.org%3E
			// "Don't bother closing it - when transactional, close is meaningless (and harmless).""
//			fsysDSet.close(); 
//			inMemDSet.close();
		}
	}
		
import com.hp.hpl.jena.query.{ ResultSet, Dataset, DatasetFactory, QuerySolution, QuerySolutionMap }
import com.hp.hpl.jena.sparql.core.{DatasetChanges,QuadAction,DatasetGraphMonitor};	
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.rdf.model.{ Model, Resource, Literal }
import com.hp.hpl.jena.shared.PrefixMapping
import com.hp.hpl.jena.sparql.modify.request.{ UpdateCreate, UpdateLoad }
import com.hp.hpl.jena.sparql.sse.SSE
import com.hp.hpl.jena.update.{ GraphStore, GraphStoreFactory, UpdateAction, UpdateRequest, UpdateFactory, Update }
/*
 * UpdateRequest request = UpdateFactory.create() ;
request.add("DROP ALL")
       .add("CREATE GRAPH <http://example/g2>")
       .add("LOAD <file:etc/update-data.ttl> INTO <http://example/g2>") ;

// And perform the operations.
UpdateAction.execute(request, graphStore) ;
but be aware that each operation added needs to be a complete SPARQL Update operation, including prefixes if needed.
 */		
		// Some old test code copied and modified from where it has languished in CC-RepoTester and Appd-DatabaseRepo
		
		// It looks like we do not have old examples of using ARQ's UpdateFactory, except in Joseki.
		// https://jena.apache.org/documentation/javadoc/arq/com/hp/hpl/jena/update/UpdateFactory.html
  def loadFromFile(dset : Dataset, srcUrlTxt : String, graphNameUriTxt : String) = {
	  
		//	updateRequest = UpdateFactory.create(querystr);
	 val gstore =  GraphStoreFactory.create(dset) ;
	 
    // A sequence of operations
    val upSpec: UpdateRequest = new UpdateRequest();

    // Create a named graph
    val creReq: UpdateCreate = new UpdateCreate(graphNameUriTxt);

    // Load a file into a named graph - NB order of arguments (both strings).
    val loadReq: UpdateLoad = new UpdateLoad(srcUrlTxt, graphNameUriTxt);  //"etc/update-data.ttl", graphName);

    // Add the two operations and execute the request
    //@SuppressWarnings Deprecated
    upSpec.add(creReq)
    upSpec.add(loadReq)

    // Execute 
    UpdateAction.execute(upSpec, gstore);

    // Print it out (format is SSE <http://jena.hpl.hp.com/wiki/SSE>)
    // used to represent a dataset.
    // Note the empty default, unnamed graph
    // SSE.write(gtore);
  }		
	

	def applyUpdateText(dset : Dataset, supTxt : String) {

		val upReq : UpdateRequest = UpdateFactory.create(supTxt);
		
		val ops : java.util.List[Update] = upReq.getOperations
		
		info1("opsList = {}", ops)
		
		val gstore =  GraphStoreFactory.create(dset) ;		
		 UpdateAction.execute(upReq, gstore);
	}

	
	def  makeMonitoredDataset(baseDSet : Dataset) : Dataset = {
		val baseDSetGraph = baseDSet.asDatasetGraph
		//  Prints:   com.hp.hpl.jena.tdb.transaction.DatasetGraphTransaction
		info1("baseDsetGraph class is: {}", baseDSetGraph.getClass)
        val dcListener = new DCListener();
        val monitoringDsetGraph = new DatasetGraphMonitor(baseDSetGraph, dcListener);			
		val wrapperDataset = DatasetFactory.create(monitoringDsetGraph) ;
		wrapperDataset
	}
	class DCListener extends VarargsLogging with DatasetChanges {
		 override def start() {
			 info0("Got start")
		 }
		 override def change(qaction : QuadAction, g : Node, s : Node, p: Node, o: Node) {
			 info2("Got change: {} with subject {}", qaction, s)
		 }
		 override def finish() { 
			 info0("Got finish")
		 }
	}
	
}

/*
 *GraphStore "A collection of graphs that an update can be applied to. 
 *The collection is one unnamed graph and zero or more named graphs, like a SPARQL dataset."
 *adds these methods to a DatasetGraph
 *void	finishRequest() - Signal end of a request being executed
void	startRequest() - Signal start of a request being executed
Dataset	toDataset() - Convert to a dataset (for query)
DatasetGraph - "DatasetGraph: The graph representation of an RDF Dataset. See Dataset for the Model level, read-only 
view (fixed set of models - the models themselves are still mutable) of an RDF dataset.
Whether a dataset contains a graph if there are no triples is not defined; see the specifc implementation. 
Some datasets are "open" - they have all graphs even if no triples, Some datasets are "closed" - fixed set of graphs"
 *
               UpdateRequest updateRequest = UpdateFactory.create(querystr);
               GraphStore graphStore = GraphStoreFactory.create(m_dataset) ;
               UpdateAction.execute(updateRequest, graphStore);
			   
 *Class UpdateRequest
     public static void execUpdate(String sparqlUpdateString,
            GraphStore graphStore) {
        UpdateRequest request = UpdateFactory.create(sparqlUpdateString);
        UpdateProcessor proc = UpdateExecutionFactory.create(request,
                graphStore);
        proc.execute();
    }

    private void updateTriple(String sparqlUpdateString) {

        dataset.begin(ReadWrite.WRITE);
        try {
            GraphStore graphStore = GraphStoreFactory.create(dataset);
            execUpdate(sparqlUpdateString, graphStore);
            dataset.commit();

        } finally {
            dataset.end();
        }
    }    
 *
UpdateRequest request = UpdateFactory.create() ;
request.add("DROP ALL")
       .add("CREATE GRAPH <http://example/g2>")
       .add("LOAD <file:etc/update-data.ttl> INTO <http://example/g2>") ;

// And perform the operations.
UpdateAction.execute(request, graphStore) ;
but be aware that each operation added needs to be a complete SPARQL Update operation, including prefixes if needed.

 
public static UpdateProcessor create(UpdateRequest updateRequest,
                     GraphStore graphStore,
                     Binding inputBinding,
                     Context context)
Create an UpdateProcessor appropriate to the GraphStore, or null if no available factory to make an UpdateProcessor
Parameters:
updateRequest -
graphStore -
inputBinding - Initial binding to be applied to Update operations that can apply an initial binding (i.e. UpdateDeleteWhere, UpdateModify)
context - (null means use merge of global and graph store context))
Returns:
UpdateProcessor or null
*/
/*
 * http://jena.apache.org/documentation/serving_data/#use-from-java
 * Use from Java
SPARQL Query
ARQ's QueryExecutionFactory.sparqlService can be used.

SPARQL Update
See UpdateExecutionFactory.createRemote

SPARQL HTTP
See DatasetAccessor
--------------------------------------------------------------------------------------------------------------------
 Fuseki Server and TDB
Fuseki include a built-in version of TDB. Run the server with the --desc argument

fuseki-server --desc tdb.ttl /ds
and a database in the directory DB, an assembler description of:

@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ja:      <http://jena.hpl.hp.com/2005/11/Assembler#> .
@prefix tdb:     <http://jena.hpl.hp.com/2008/tdb#> .

[] ja:loadClass "com.hp.hpl.jena.tdb.TDB" .
tdb:DatasetTDB  rdfs:subClassOf  ja:RDFDataset .
tdb:GraphTDB    rdfs:subClassOf  ja:Model .

<#dataset> rdf:type      tdb:DatasetTDB ;
     tdb:location "DB" ;
     .
The form:

fuseki-server --loc=DB /ds
is a shorthand for such an assembler with location DB.

To make triples from all the named graphs appear as the default, unnamed graph, use:

<#dataset> rdf:type      tdb:DatasetTDB ;
     tdb:location "DB" ;
     tdb:unionDefaultGraph true ;
    .
Fuseki Server and general dataset descriptions
The Fuseki server can be given an assembler description to build a variety of model and datasets types.

fuseki-server --desc assembler.ttl /ds
Full details of setting up models assembler is given in the assembler documentation.

A general dataset is described by:

# Dataset of default graph and one named graph.
<#dataset> rdf:type ja:RDFDataset ;
   ja:defaultGraph <#modejDft> ;
   ja:namedGraph
       [ ja:graphName      <http://example.org/name1> ;
         ja:graph          <#model1> ] ;
   .

<#modelDft> a ja:MemoryModel ;
        ja:content [ ja:externalContent <file:Data.ttl> .

<#model1>  rdf:type ja:MemoryModel ;
   ja:content [ ja:externalContent <file:FILE-1.ttl> ] ;
   ja:content [ ja:externalContent <file:FILE-2.ttl> ] ;
   .
The models can be Jena inference models.
 
 
 */
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

import org.appdapter.core.store.{ Repo, BasicRepoImpl, BasicStoredMutableRepoImpl, InitialBinding, ModelClient }

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


