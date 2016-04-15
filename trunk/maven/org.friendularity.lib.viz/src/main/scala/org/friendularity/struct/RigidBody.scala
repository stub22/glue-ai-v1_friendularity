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

object RigidBodyFieldNames {
	val	(pos, dir) = ("pos", "dir")
	val aodf = new	AODFactory(1)
	val pos3Factory = new AODFactory(3)
	val dir4Factory = new AODFactory(4)
}
class RigidBodyStruct extends DirectlyFactoriedStruct[String, ArrayOfDoubles](RigidBodyFieldNames.aodf) {
	override protected def makeFieldVal(fk : String) : ArrayOfDoubles = {
		fk match {
			case RigidBodyFieldNames.pos => RigidBodyFieldNames.pos3Factory.make()
			case RigidBodyFieldNames.dir => RigidBodyFieldNames.dir4Factory.make()
		}
	}
}
// class SnapTimes extends Struct[FK, FV] {	
// }
class SnapStruct extends DirectlyFactoriedStruct[String, ArrayOfDoubles](RigidBodyFieldNames.aodf) {
}