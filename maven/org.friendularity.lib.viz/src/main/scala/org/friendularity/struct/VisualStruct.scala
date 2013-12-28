/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.math.Quaternion;
/**
 * @author Stu B. <www.texpedient.com>
 */

class VisualStruct {

}

class Vec3fFactory extends Factory[Vector3f] {
	override def make() : Vector3f = {
		new Vector3f()
	}
	override def makeArray(size : Int) : Array[Vector3f] = {
		new Array[Vector3f](size)
	}
	override def shallowCopyContents(source : Vector3f, target: Vector3f) {
		target.set(source)
	}
}
object JME3Factorys {
	val vec3fFactory = new Vec3fFactory()
}

class Vec3fBasicStruct[FK] extends BasicStruct[FK, Vector3f](JME3Factorys.vec3fFactory) 
{
	// Represents any number of Vector3fs, keyed by any FK (String, Ident, Enum, ...)
	// Does not provide *direct* access to the underlying .x,.y,.z  fields of each vector.
} 
class Vec3fWrapperStruct[FK](val wrapped : Struct[FK, ArrayOfDoubles]) extends Struct[FK, Vector3f] {
	// Represents any number
	// reused buffer is not threadsafe
	val	myBuffer : ArrayOfDoubles = RigidBodyFieldNames.pos3Factory.make()
	override def writeField(fk : FK, dVal : Vector3f) {
		myBuffer.myVals(0)  = dVal.x
		myBuffer.myVals(1)  = dVal.y
		myBuffer.myVals(2)  = dVal.z
		wrapped.writeField(fk, myBuffer)
	}
	override def readField(fk : FK, dVal : Vector3f) {
		wrapped.readField(fk, myBuffer)
		dVal.x = myBuffer.myVals(0).asInstanceOf[Float]
		dVal.y = myBuffer.myVals(1).asInstanceOf[Float]
		dVal.z = myBuffer.myVals(2).asInstanceOf[Float]
	}	
}