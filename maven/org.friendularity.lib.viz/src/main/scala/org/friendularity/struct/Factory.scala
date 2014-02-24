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

import org.friendularity.api.struct.Maker;


/* 
 found   : Array[Elem with Object]
 required: Array[Elem]
 Note: Elem with Object <: Elem, but class Array is invariant in type T.
 You may wish to investigate a wildcard type such as `_ <: Elem`. (SLS 3.2.10)
 val myElems : Array[Elem] = myElemFactory.makeArray(mySize)
 http://stackoverflow.com/questions/10000126/re-using-java-generic-collections-in-scala-without-trait-object	
 */
abstract class Factory[V] extends Maker[V] {
	// If this were a trait instead of abstract class, then it would not be properly extensible 
	// from Java, because it contains method impls.
	// If V represents any kind of an array, then make() must know how big that array is.
	def make() : V = makeOne()
	def makeArray(size : Int) : Array[V] 
}
trait FieldMaker[FK, DV <: DataValue] {
	// Unused
	def makeField(fieldKey : FK) : DV
}
