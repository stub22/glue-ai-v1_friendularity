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

package org.friendularity.struct
trait DataExpr {
	// Marker trait that we might not really need
}
trait DataValue {
	// Marker trait that we might not really need
}
// Essentially an evaluator that knows how to (repeatedly) process the DE and send the result a target DV
trait DataSource[DE <: DataExpr, DV <: DataValue] {
	def read(expr : DE, dataValue : DV)
}
// A Binding attaches a particular expression to a value-buffer, but leaves the DataSource floating until eval-time.
// The expression may be evaluated many times, overwrite-updating the value in the buffer each time.
// The buffer is an unsafe temp-buffer for us to use in bridging between data-source and (all) structs
class DataBinding[DE <: DataExpr, BDV <: DataValue](val myExpr : DE, val myBufferDV : BDV) {
	// Here at eval-time we finally must have a data-soruce
	def readSourceField(ds : DataSource[DE, BDV]) : Unit = {
		// Un-threadsafe use of temp buffer val
		ds.read(myExpr, myBufferDV)
	}
}
