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
 * 
 * If a Fuseki server is run with the command:

fuseki-server --mem /dataset
then the service endpoints are:

HTTP: http://localhost:3030/dataset/data
Query: http://localhost:3030/dataset/query
Update: http://localhost:3030/dataset/update

 
 */

import com.hp.hpl.jena.query.{DatasetAccessor, Dataset, DatasetAccessorFactory}

import org.appdapter.core.name.{Ident, FreeIdent}

object TestLoops {
	def main(args: Array[String]) : Unit = {
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);
		
		val serviceUrl = "http://lima.nodeset.com:4001/dstst_4001/data"
		val dAcc = DatasetAccessorFactory.createHTTP(serviceUrl)
		
		val jacc = new JenaArqCheckoutConn(dAcc)
		
		val gnBase = "urn:ftd:cogchar.org:20140701:runtime#graph_7"
		val gn01 = gnBase + "01"
		val gn02 = gnBase + "02"
		
		val gid01 = new FreeIdent(gn01)
		val gid02 = new FreeIdent(gn02)
		val ck01 = jacc.makeCheckoutHandle(gid01)
		val ck02 = jacc.makeCheckoutHandle(gid02)
		
		ck01.refreshCheckout
		
		val rm01 = ck01.getAsReactorModel
		val rm02 = ck02.getAsReactorModel
		
		import org.friendularity.gen.reacted.lpath._
		
		rm01.open()
		
		val studFol_A = new StudentFolio(rm01, true)
		val studFol_B = new StudentFolio(rm01, false)
		
		studFol_A.addComment("Hey everybody, ha cha")
		studFol_B.addComment("Noone cares, sniff")
		
		println("After Folios created, rm01=" + rm01.toString)
		println("rm01.size=" + rm01.size)

		ck01.checkinAsReplace
		
		println("Finished checkin to " + ck01)
		
	}
}
