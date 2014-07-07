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
		
		// The server dataset we are accessing (for both read + write)
		val serviceUrl = "http://lima.nodeset.com:4001/dstst_4001/data"
		
		// Create an accessor object for that service - stateless as far as we know
		val dAcc = DatasetAccessorFactory.createHTTP(serviceUrl)
		
		// Make a little accessor wrappor guy giving us some skeletal "checkout" semantics.
		val jacc = new JenaArqCheckoutConn(dAcc)

		// Namespace for our current target graphURIs 
		val nsOpenGraph_july2014 = "urn:ftd:friendularity.org:201407:graph.open.mdirtst#"
		
		// Namespace for our current mdir metadata *instance* records, grounded in the Mdir ontology (which has its
		// own, different namespace).
		val ns_gmdinst = "urn:fdc:friendularity.org:2014:gmdinst#"
		
		// A particular source/target graph URN we will create/update for test purposes.
		val gn_mdm = nsOpenGraph_july2014 + "metaDataTest_803";
		
		val gid_md = new FreeIdent(gn_mdm)
		
		// Create an (initially-empty) local checkout model 
		val ck_md = jacc.makeCheckoutHandle(gid_md)
		// Populate it with existing contents of the given graph.
		ck_md.refreshCheckout
		
		// Now let's access+update the contents of the local checkout graph, using the RDF-Reactor generated 
		// wrapper classes.
		// First we need a handle to an RDF2Go model wrapper.
		val mdm01 : org.ontoware.rdf2go.model.Model = ck_md.getAsReactorModel
		mdm01.open()
		val r2goModelWrapper = new R2GoModelWrapper(mdm01)
		// How large is the graph before we start updating it?
		val beforeUpdatesSize = mdm01.size
		println("Before updates started, mdm01.size=" + beforeUpdatesSize)
		
		// Temporary promiscuous import during prototype development phase.
		import org.friendularity.gen.reacted.mdir._
		
		// So far we have 4 test bindings below, using the following input test data, which describes one of our
		// legacy test data spreadsheets.
		val sheetKeyA = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc" 
		val namespaceTabNum = 9
		val dirTabNum = 8

		// 1) Bind a host spreadsheet object 
		val urn_sheetHost_A = ns_gmdinst + "host4sheet_testy_A"
		val hostSheet_A = new GH4SSpreadsheet(mdm01, urn_sheetHost_A, true);
		
		hostSheet_A.setComment("Host record for spreadsheet GluePuma_HRKR50_TestFull")
		hostSheet_A.setSpreadsheetKey(sheetKeyA)
		val fragTail_a8Dir = "testy_A8_dir"
		// 2) Bind a host for the "dir" tab within that host sheet
		val urn_tabHost_A8 = ns_gmdinst + "host4tab_" + fragTail_a8Dir
		val hostTab_A8 = new GH3STabInSpreadsheet(mdm01, urn_tabHost_A8, true);
		hostTab_A8.setComment("Reading from the dir model in tab 8 of GluePuma_HRKR50_TestFull at YYYY-MM-DD ")
		hostTab_A8.setParentHost4Quads(hostSheet_A)
		hostTab_A8.setTabNumber(dirTabNum)

		// 3) Bind an NVPair chunk for the "ns" tab within that host sheet
		val urn_nsTab_A8 =  ns_gmdinst + "nsChunkTab_testy_A9_ns"		
		val nsTab_A9 = new NVPairTabInSheet(mdm01, urn_nsTab_A8, true)

		val fragPrefix_gptr = "gptr."
		val fragPrefix_gptrOpen =  fragPrefix_gptr + "open."
		// 4) Now let's make a graphPointer referring to the dir-graph host.  
		// By convention we mark the fragment with "gptr.open" to signify an open gptr.
		val urn_graphPtr_toA8dir = ns_gmdinst + fragPrefix_gptrOpen  + fragTail_a8Dir;
		val opnGPtr_toA8dir = new GPOpen(mdm01, urn_graphPtr_toA8dir, true);
		opnGPtr_toA8dir.setPointsToGraphHost(hostTab_A8)
		// val uri = opnGPtr_toA8dir.get
		
		val optBo : Option[GH3STabInSpreadsheet] = r2goModelWrapper.getSingleBoundObj(opnGPtr_toA8dir, GraphPointer.POINTSTOGRAPHHOST, classOf[GH3STabInSpreadsheet])
	
		println("Fetched optFo: " + optBo)
		val innerBo :GH3STabInSpreadsheet = optBo.get
		println("innerBo class=" + innerBo.getClass)
		println("innerBo tabNums=" + innerBo.getAllTabNumber_as.asList)
		println("innerBo tabNums.first=" + innerBo.getAllTabNumber_as.firstValue)
		val afterUpdatesSize = mdm01.size
		println("After Host-recs created, model size=", afterUpdatesSize, " net change=", afterUpdatesSize - beforeUpdatesSize )
		println("Replacing stored contents of graph")
		ck_md.checkinAsReplace
		
		println("Finished checkin to " + ck_md)
		
	}
}
import org.ontoware.rdfreactor.runtime.Base;
class R2GoModelWrapper(val myRdf2goModel : org.ontoware.rdf2go.model.Model) {
	def getSingleBoundObj [BT <: org.ontoware.rdfreactor.schema.rdfs.Class](r : org.ontoware.rdf2go.model.node.Resource, 
						p : org.ontoware.rdf2go.model.node.URI, desiredClass : Class[BT]): Option[BT] = {
		val found : BT = Base.get(myRdf2goModel, r, p, desiredClass).asInstanceOf[BT]
		if (found != null) {
			Some(found)
		} else {
			None
		}
	}
	
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