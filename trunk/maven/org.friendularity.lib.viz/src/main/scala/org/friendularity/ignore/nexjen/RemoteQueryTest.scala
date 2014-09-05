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

import scala.xml.{NodeSeq,Text}
import org.appdapter.fancy.gportal.{GraphPortal, GraphSupplier, GraphQuerier, QueryParseHelper, SuppliedGraphStat, GraphPortalFuncs, DelegatingPortal}
import com.hp.hpl.jena.query.{ Dataset, DatasetFactory, ReadWrite , Query, QueryExecution, QuerySolution}
import com.hp.hpl.jena.rdf.model.{ Resource, RDFNode, Literal}

/**
 * @author Stu B. <www.texpedient.com>
 */

object RemoteQueryTest {
	lazy val sheetSpec_L5SG =  ExImBank.spec_testData_majL5SG();
	lazy val folderSpec_headless = ExImBank.spec_headlessR25();
	lazy val folderSpec_oglwebGlue = ExImBank.spec_oglwebGlue();
	lazy val folderSpec_oglwebR25 = ExImBank.spec_oglwebR25();

	lazy val llp_L5SG : DelegatingPortal = ExImBank.loadLegacyLocalPortal(sheetSpec_L5SG)
	lazy val llp_headless : DelegatingPortal = ExImBank.loadLegacyLocalPortal(folderSpec_headless)
	lazy val llp_oglwebGlue : DelegatingPortal = ExImBank.loadLegacyLocalPortal(folderSpec_oglwebGlue)
	lazy val llp_oglwebR25 : DelegatingPortal = ExImBank.loadLegacyLocalPortal(folderSpec_oglwebR25)
	
	lazy val  parsedQueryDeal : Query = QueryParseHelper.parseQuery(bigOlQueryString)
	
	def main(args: Array[String]) : Unit = {
		org.appdapter.bind.log4j.Log4jFuncs.forceLog4jConfig(new java.net.URL("file:src/main/resources/log4j.properties"));
		
		// val anzd_headlessR25 = ExImBank.loadAndAnalyze(folderSpec_headless)
		// ExImBank.printStuff(anzd_headlessR25)
		
		val sparqlXmlOut = doSparqlStuff
		println("sparqlOut: " + sparqlXmlOut)
		
		
	}
	def doSparqlStuff  : NodeSeq = {
		// QueryExec can only be used once.
		
		val querier = llp_oglwebR25.getQuerier
		
		val queryExec : QueryExecution = querier.makeQueryExec(parsedQueryDeal)
		val qsols : List[QuerySolution] = querier.gulpingSelect_ReadTransCompatible(queryExec)
		var numAnswers : Int = 0
		val tableOutXml : NodeSeq = <table><tr><th>Type</th><th>Subject</th><th>Property</th><th>Stmt Cnt</th><th>MIN(Obj)</th></tr> {
			for (qs <- qsols) yield {

				numAnswers += 1
				val tRes = qs.getResource("t")
				val gRes = qs.getResource("g")
				val sRes = qs.getResource("s")
				val pRes = qs.getResource("p")
				val minObjNode : RDFNode = qs.get("minO")
				val tLabel = tRes.getLocalName
				val gUri = gRes.getURI
				val gLabel = gRes.getLocalName
				val sLabel = sRes.getLocalName
				val pLabel = pRes.getLocalName
				val stmtCnt = qs.getLiteral("cntO").getInt
				val rowID : String = "r" + numAnswers
				<tr id={rowID}>
					<td id={rowID + "_type"}>{tLabel}</td>
					<td id={rowID + "_subj"}>{sLabel}</td>
					<td id={rowID + "_prop"}>{pLabel}</td>
					<td id={rowID + "_smts"}>{stmtCnt}</td>
					<td id={rowID + "_minObj"}>{minObjNode.toString}</td>
				</tr>
			}	
		} </table>
		val reportOutXml : NodeSeq =    <div>
											<pre>
{bigOlQueryString}
											</pre>
											<br/><hr/><span>NumAnswers: {numAnswers}</span><br/>
											{tableOutXml}
										</div>
		
		reportOutXml
	}

	def makeTableFromSparqlResults : NodeSeq = {
		val queryText = RemoteQueryTest.bigOlQueryString
		
		<table>
			<tr><td> {
				queryText
			}
			</td>
			</tr><tr>
				<td>One</td>
				<td>Two</td>
			</tr>
		</table>
	}	
	
val bigOlQueryString = """
PREFIX  rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX  rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX  xsd: <http://www.w3.org/2001/XMLSchema#>

PREFIX  ccr: <urn:ftd:cogchar.org:2012:runtime#>
PREFIX  csi: <http://www.cogchar.org/schema/scene/instance#>
PREFIX  cst: <http://www.cogchar.org/schema/scene#>

SELECT   ?t ?g ?s ?p (MIN(?o) AS ?minO) (COUNT(?o) AS ?cntO)
   WHERE { GRAPH ?g  {?s rdf:type ?t;  ?p ?o.}
   VALUES ?t {cst:BuildableSceneSpec  cst:BuildableBehaviorSpec  cst:BuildableChannelSpec}
}
  GROUP BY ?t ?g ?s ?p 
  ORDER BY DESC(?cntO)
"""
}
