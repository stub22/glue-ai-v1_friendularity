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

class OtherRingBuf[Elem](val mySize : Int, val myElemFactory : Factory[Elem]) {
	val myElems : Array[Elem] = myElemFactory.makeArray(mySize)
	var	myCurrentIndex = 0
	// Do we like having empty elements initially? 
	for (idx <- 0 to mySize-1) {
		myElems(idx) = myElemFactory.make()
	}
	def getCurrent() : Elem = myElems(myCurrentIndex)
	
	def advance() {
		myCurrentIndex = (1 + myCurrentIndex) % mySize
	}
	// 0 = current
	def getPrevious(howManyBack : Int) : Elem = {
		// Java can return negative remainders - this formula corrects to always give a positive modulus.
		val prevIndex = (((myCurrentIndex - howManyBack) % mySize) + mySize) % mySize;
		myElems(prevIndex)
	}
	// Considering this, where howMany <= mySize
	// Order is reversed: 
	// result[0] = current, result[1] = previous.
	
	
	// def getCurrent
}
