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
		
		val serviceUrl = "http://lima.nodeset.com:4001/dstst_4001/data"
		val dAcc = DatasetAccessorFactory.createHTTP(serviceUrl)
		
		val jacc = new JenaArqCheckoutConn(dAcc)
		
		val mdmGnBase = "urn:ftd:cogchar.org:20140701:runtime:open#graph_8"
		
		val gn_mdm = mdmGnBase + "01"
		
		
// 		val gn02 = gnBase + "02"
		
		val gid_md = new FreeIdent(gn_mdm)
//		val gid02 = new FreeIdent(gn02)
		val ck_md = jacc.makeCheckoutHandle(gid_md)
// 		val ck02 = jacc.makeCheckoutHandle(gid02)
		
		ck_md.refreshCheckout
		
		val mdm01 : org.ontoware.rdf2go.model.Model = ck_md.getAsReactorModel

		import org.friendularity.gen.reacted.mdir._

		val gn_sheetHost_01 = "urn:ftd:cogchar.org:20140701:runtime:open#sheetHost_987"
		
		mdm01.open()
		
		val hostSheet_A = new HostSpreadsheet(mdm01, gn_sheetHost_01, true);
		
		hostSheet_A.addComment("A known spreadsheet is commented upon here")
/*
		
		studFol_B.setPoints(22)  //      xsd:integer in schema, but xsd:int in asserted value
		studFol_B.setStatDouble(19.3423425)
		studFol_A.setStatFloat(0.3f)
		studFol_A.addFlag(true)
		studFol_A.addFlag(false)
		studFol_A.setFlag(true)
*/		
		println("After Host-recs created, rm01.size=" + mdm01.size)

		ck_md.checkinAsReplace
		
		println("Finished checkin to " + ck_md)
		
	}
}