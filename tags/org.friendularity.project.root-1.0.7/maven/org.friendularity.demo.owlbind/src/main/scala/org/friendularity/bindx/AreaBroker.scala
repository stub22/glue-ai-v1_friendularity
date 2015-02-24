/*
 *  Copyright 2015 by The Friendularity Project (www.friendularity.org).
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
 * @author stub22
 * 
 * AreaBroker knows about graph processing in some functional area. 
 * Two main sub-kinds:  Feature and Meta.
 * FeatureABs provide a usable functional view for some business use case.
 * MetaAB is used to store config for a bunch of FeatureABs.  
 * It is usually configured from a small bootstrap.ttl file, with an
 * entry for each of the FeatureABs to be provided.  Instantiation of
 * the actual FeatureABs may be greedy or lazy.  
 * 
 * These ABs are able to create runtime in-memory index information from 
 * available resources, in order to minimize the amount of index stored 
 * to disk.
 * 
 * Each AreaBroker should try to avoid assuming it is the only AreaBroker of its kind.
 * It knows that other brokers may be accessing + modifying graphs that it cares about.   
 * Commonly we voluntarily use transactions to protect components from each other,
 * but no single absolute pattern or authority is specified at this time.
 * 
 *  Examples of functional areas:
 *  
 *  authored + generated
 *		Config [of kind X] - (bounded by the constrasting Content, Media, Meta, ...)
 *		Content [X] - authored + generated
 *		MediaPointers - (all our authored/managed refs to non-graph data go here)
 *		
 *	dynamic 	
 *		RuntimeState[X] - usually in-memory only
 *		ResponseLog - scaled appropriately for response volume
 *		
 *	offline / integ
 *		ArchivedResults - 
 *		UserAccounts - 
 *		
 *	bootstrap
 *		MetaDirectory - locates broker-config graphs for the above and other/sub feature areas.
 *				Treated as read-only by app.  Updates occur only through side-effects of requests.
 *				Not fully persistent to disk.  
 *	-------------------
 *	
 *	Different area brokers use different graph pattern combinations (circuses).
 *		
 */

trait AreaBroker {
}
// "Meta" information is not a "feature" area, but everything else is.
trait FeatureAB { 
	// Configured by onto-type cr:BR_Feature
}
trait MetaAB {
	// Configured by onto-type cr:BR_Meta
}

// Uses backing of a single private in-memory graph of MDir records, which contains some GraphPointers and CircusRecipes.
class PrivateMemGraphAB extends AreaBroker {
	def one() = ???
}

class OneDatasetAB



