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

import com.hp.hpl.jena.query.{ Dataset, DatasetFactory }

import com.hp.hpl.jena.tdb.TDBFactory
import com.hp.hpl.jena.query.ReadWrite
import com.hp.hpl.jena.sparql.util.Context

import org.apache.jena.riot.RDFDataMgr

import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo}
import org.appdapter.core.query.{ InitialBinding }
import org.appdapter.fancy.rclient.{RepoClient, RepoClientImpl}
import org.appdapter.fancy.query.{InitialBindingImpl}

/**
 * @author Stu B. <www.texpedient.com>
 */

class TxRepo(val myRepoID : Ident) {
	
}
class TxOper (val myRecordID : Ident, val myOriginRepoID : Ident) {
	
}

import com.hp.hpl.jena.query.{ ResultSet, Dataset, DatasetFactory, QuerySolution, QuerySolutionMap }
import com.hp.hpl.jena.sparql.core.{DatasetChanges,QuadAction,DatasetGraphMonitor};	
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.rdf.model.{ Model, Resource, Literal }
import com.hp.hpl.jena.shared.PrefixMapping
import com.hp.hpl.jena.sparql.modify.request.{ UpdateCreate, UpdateLoad }
import com.hp.hpl.jena.sparql.sse.SSE
import com.hp.hpl.jena.update.{ GraphStore, GraphStoreFactory, UpdateAction, UpdateRequest, UpdateFactory }

class FriendlyTxRepo  {
 

	def loadFromFile(dset : Dataset, srcUrlTxt : String, graphNameUriTxt : String) = {
		val upSpec: UpdateRequest = new UpdateRequest();	  
		//	updateRequest = UpdateFactory.create(querystr);
		val gstore =  GraphStoreFactory.create(dset) ;
		// These are *Update* objects to be added to the single UpdateRequest 
		val creReq: UpdateCreate = new UpdateCreate(graphNameUriTxt);
		// Load a file into a named graph - NB order of arguments (both strings).
		val loadReq: UpdateLoad = new UpdateLoad(srcUrlTxt, graphNameUriTxt);	

		// Add the two operations and execute the request
		//@SuppressWarnings Deprecated
		upSpec.add(creReq)
		upSpec.add(loadReq)

		// Execute 
		UpdateAction.execute(upSpec, gstore);		
	}
	def applyUpdateText(dset : Dataset, supTxt : String) {

		val upReq : UpdateRequest = UpdateFactory.create(supTxt);
		val gstore =  GraphStoreFactory.create(dset) ;		
		UpdateAction.execute(upReq, gstore);
	}	
}