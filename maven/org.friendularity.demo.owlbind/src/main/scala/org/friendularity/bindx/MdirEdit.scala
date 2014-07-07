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
		// How large is the graph before we start updating it?
		val beforeUpdatesSize = mdm01.size
		println("Before updates started, mdm01.size=" + beforeUpdatesSize)
		
		// Temporary promiscuous import during prototype development phase.
		import org.friendularity.gen.reacted.mdir._
		
		// So far we have 3 test bindings below, using the following input test data, which describes one of our
		// legacy test data spreadsheets.
		val sheetKeyA = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc" 
		val namespaceTabNum = 9
		val dirTabNum = 8

		// 1) Bind a host spreadsheet object 
		val urn_sheetHost_A = ns_gmdinst + "host4sheet_testy_A"
		val hostSheet_A = new GH4SSpreadsheet(mdm01, urn_sheetHost_A, true);
		
		hostSheet_A.setComment("Host record for spreadsheet GluePuma_HRKR50_TestFull")
		hostSheet_A.setSpreadsheetKey(sheetKeyA)
		
		// 2) Bind a host for the "dir" tab within that host sheet
		val urn_tabHost_A8 = ns_gmdinst + "host4tab_testy_A8_dir"
		val hostTab_A8 = new GH3STabInSpreadsheet(mdm01, urn_tabHost_A8, true);
		hostTab_A8.setComment("Reading from the dir model in tab 8 of GluePuma_HRKR50_TestFull at YYYY-MM-DD ")
		hostTab_A8.setParentHost4Quads(hostSheet_A)
		hostTab_A8.setTabNumber(dirTabNum)

		// 3) Bind an NVPair chunk for the "ns" tab within that host sheet
		val urn_nsTab_A8 =  ns_gmdinst + "nsChunkTab_testy_A9_ns"		
		val nsTab_A9 = new NVPairTabInSheet(mdm01, urn_nsTab_A8, true)

	
		val afterUpdatesSize = mdm01.size
		println("After Host-recs created, model size=", afterUpdatesSize, ", net change=", afterUpdatesSize - beforeUpdatesSize )
		println("Replacing stored contents of graph")
		ck_md.checkinAsReplace
		
		println("Finished checkin to " + ck_md)
		
	}
}