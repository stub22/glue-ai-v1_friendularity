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

package org.friendularity.bindx

/**
 * @author Stu B. <www.texpedient.com>
 */


import com.hp.hpl.jena.query.{DatasetAccessor, Dataset, DatasetAccessorFactory}

import org.appdapter.core.name.{Ident, FreeIdent}

object MdirEdit {
	def main(args: Array[String]) : Unit = {
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);

		// A scratchpad server that noone should rely on for anything.  Could contain in-mem or persist - who knows?
		val dummyTest_ServerUrl = "http://lima.nodeset.com:4001"
		
		// The first persistent cataloging server, including metadata and working dev character data, 
		// mainly imported from serial forms
		val persistDev_ServerUrl = "http://lima.nodeset.com:4002"

		// The test data-data server dataset we are importing data into (to be moved to pdev_4002)
		val dummyTest_ServiceUrl =  dummyTest_ServerUrl +  "/dstst_4001/data"

		val mdirReal_ServiceUrl = persistDev_ServerUrl + "/mdir_4002/data"
		val pdevReal_ServiceUrl = persistDev_ServerUrl + "/pdev_4002/data"

		// Namespace for our current target graphURIs 
		val nsOpenMdir_july2014 = "urn:ftd:friendularity.org:201407:graph.open.mdirtst#"
		val nsOpenLessonDev_july2014 = "urn:ftd:friendularity.org:201407:graph.open.lessdev#"
		
		// Namespace for our current mdir metadata *instance* records, grounded in the Mdir ontology (which has its
		// own, different namespace).
		
		
		val testSeqNum = "805"
		// Our working metadata graph URI
		val gn_mdm = nsOpenMdir_july2014 + "metaDataTest_" + testSeqNum;
		// Our first import target graph URI
		val gn_dirImport = nsOpenLessonDev_july2014 + "dirImp_" + testSeqNum;

		// Create an accessor object for that service - stateless as far as we know
		val dAcc = DatasetAccessorFactory.createHTTP(dummyTest_ServiceUrl)
		
		// Make a little accessor wrappor guy giving us some skeletal "checkout" semantics.
		val jacc = new JenaArqCheckoutConn(dAcc)
		
		val gid_md = new FreeIdent(gn_mdm)
		
		// Create an (initially-empty) local checkout model 
		val ck_md = jacc.makeCheckoutHandle(gid_md)
		
		val mdirBinder = new MDirBinder(ck_md);
	//	val ns_gmdinst =  mdirBinder.ns_gmdinst  // "urn:fdc:friendularity.org:2014:gmdinst#"
		// Populate it with existing contents of the given graph.
		ck_md.refreshCheckout
		
		// Now let's access+update the contents of the local checkout graph, using the RDF-Reactor generated 
		// wrapper classes.
		// First we need a handle to an RDF2Go model wrapper.
		val mdm01 : org.ontoware.rdf2go.model.Model = mdirBinder.getR2goModel
		mdm01.open()
		// val r2goModelWrapper = new R2GoModelWrapper(mdm01)
		// How large is the graph before we start updating it?
		val beforeUpdatesSize = mdm01.size
		println("Before updates started, mdm01.size=" + beforeUpdatesSize)
/******/
		// Temporary promiscuous import during prototype development phase.
		import org.friendularity.gen.reacted.mdir._
		
		// Our overall goal for this test is to absorb input test data from this legacy data repo-sheet.
		val sheetKeyA = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc" 
		val namespaceTabNum = 9
		val dirTabNum = 8

		// 1) Bind a host spreadsheet object 
		val uri_sheetHost_A = mdirBinder.makeInstanceURI("host4sheet_testy_A")
		val hostSheet_A = new GH4SSpreadsheet(mdm01, uri_sheetHost_A, true);
		
		hostSheet_A.setComment("Host record for spreadsheet GluePuma_R50_TestFull")
		hostSheet_A.setSpreadsheetKey(sheetKeyA)
		val fragTail_a8Dir = "source_A8_dirTab"
		// 2) Bind a host for the "dir" tab within that host sheet
		val uri_tabHost_A8 = mdirBinder.makeInstanceURI("host3tab_" + fragTail_a8Dir)
		val hostTab_A8 = new GH3STabInSpreadsheet(mdm01, uri_tabHost_A8, true);
		hostTab_A8.setComment("Reading from the dir model in tab 8 of GluePuma_R50_TestFull at YYYY-MM-DD")
		hostTab_A8.setParentHost4Quads(hostSheet_A)
		hostTab_A8.setTabNumber(dirTabNum)

		// 3) Bind an NVPair chunk for the "ns" tab within that host sheet
		val uri_nsTab_A8 =  mdirBinder.makeInstanceURI("nsChunkTab_testy_A9_ns")
		val nsTab_A9 = new NVPairTabInSheet(mdm01, uri_nsTab_A8, true)
		hostSheet_A.setNamespaceChunkTab(nsTab_A9)

		// 4) Now let's make a graphPointer referring to the dir-graph host.  
		// By convention we mark the fragment with "gptr.open" to signify an open gptr.
		val uri_graphPtr_toA8dir = mdirBinder.makeGPOpenUri(fragTail_a8Dir);
		// This is a pointer to an importable dir sheet-graph.   It does not have a proper graphNameURI of its own.
		val opnGPtr_toA8dir = new GPOpen(mdm01, uri_graphPtr_toA8dir, true);
		opnGPtr_toA8dir.setPointsToGraphHost(hostTab_A8)
		// No GraphNameURI set - on purpose - see comment above.
		
		val optBo : Option[GH3STabInSpreadsheet] = mdirBinder.getSingleBoundObj(opnGPtr_toA8dir, GraphPointer.POINTSTOGRAPHHOST, classOf[GH3STabInSpreadsheet])
	
		println("Fetched optFo: " + optBo)
		val innerBo :GH3STabInSpreadsheet = optBo.get
		println("innerBo class=" + innerBo.getClass)
		println("innerBo tabNums=" + innerBo.getAllTabNumber_as.asList)
		println("innerBo tabNums.first=" + innerBo.getAllTabNumber_as.firstValue)
		
		val afterUpdatesSize = mdm01.size
		println("After Host-recs created, model size=", afterUpdatesSize, " net change=", afterUpdatesSize - beforeUpdatesSize )
		println("Replacing stored contents of graph")
		ck_md.checkinAsReplace
		
		println("Finished first checkin to " + ck_md)

		// 6) Now we need GHosts for the target data server (order 5) and quadstores (order 4).
		// We do not need 3rd order hosts when working with a proper quadstore server, other than
		// external import/exports hosts like the ones above.
		val uri_tgtServHost_T = mdirBinder.makeInstanceURI("ghost5serv_tgt_importTest")
		val tgtServHost5_importTest : GH5RSFusekiServer =  mdirBinder.ensureHost5FusekiServer(uri_tgtServHost_T, dummyTest_ServerUrl)
		////  new GH5RSFusekiServer(mdm01, urn_tgtServHost_T, true);
		// tgtServHost_importTest.setUrlText(dummyTest_ServerUrl)
		
		println("mdm01 now reports size is=" + mdm01.size)

		val uri_tgtChardatSvc4q = mdirBinder.makeInstanceURI("ghost4q_tchar")
		val tgtChardatSvc4q : GH4RSOHFusekiDataset = mdirBinder.ensureHost4FusekiDataset(uri_tgtChardatSvc4q, uri_tgtServHost_T, dummyTest_ServiceUrl)
		
		//    new GH4RSOHFusekiDataset(mdm01, urn_tgtChardatSvc4q, true)
		// tgtChardatSvc4q.setParentHost5Quints(tgtServHost5_importTest)
		// tgtChardatSvc4q.setUrlText(dummyTest_ServiceUrl)
		
		val uri_gpTgtImportedDir = mdirBinder.makeGPOpenUri("impDirTst")
		val opnGPtr_tgtImpDir = new GPOpen(mdm01, uri_gpTgtImportedDir, true);
		// 7) Next we need a pointer for a graph to read the old dir model into		
		// We point to the graphHost, thus essentially getting the URL for the SOH dataset access.
		// Then all we need ... is a graph Name URI!
		opnGPtr_tgtImpDir.setPointsToGraphHost(tgtChardatSvc4q) // gn_dirImport
		val r2goURI = mdirBinder.makeURI(gn_dirImport);
		opnGPtr_tgtImpDir.setGraphNameUri(r2goURI)
		
		// 8) An auditable operation recording the import attempt.
		// Has a generated blank-node for its own name, so we can use the two-args constructor.
		// This implies that a new instance will be created every time we run an object-CRUD test.

		val importOp = new GOCopyToNewOpen(mdm01, true);
		importOp.setComment("This operation records an import.  It uses a blankNode subject resource.")
		importOp.setSourceGP(opnGPtr_toA8dir)
		importOp.setTargetGP(opnGPtr_tgtImpDir)
		
		println("Final R2go reported size is=", mdm01.size)
		ck_md.checkinAsReplace
		println("Finished final checkin to " + ck_md)
				
	}
// import org.appdapter.fancy.rspec.{RepoSpec, OnlineSheetRepoSpec}

	def onlineSheetSpec() { 
		// new OnlineSheetRepoSpec(REPO_SHEET_KEY, NAMESPACE_SHEET_NUM, DIRECTORY_SHEET_NUM, emptyFileResModelCLs);
	}
}


import org.appdapter.core.store.Repo
import org.appdapter.demo.DemoResources
import org.appdapter.fancy.rclient.{RepoClient, RepoClientImpl}

class RepoMdirBinder {
	
}



/*
Here is a query to show the contents of the metaData graph read/written above,
nicely formatted using namespace prefixes.

PREFIX og: <urn:ftd:friendularity.org:201407:graph.open.mdirtst#>
PREFIX mdo: <http://onto.cogchar.org/onto/201407/MetaDir_OWL2#>
PREFIX mdi: <urn:fdc:friendularity.org:2014:gmdinst#>
PREFIX  rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX  rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX  xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?s ?p ?o 
WHERE { GRAPH og:metaDataTest_803 {?s ?p ?o} }
ORDER by ?s ?p ?o

 * 
 * 
 */