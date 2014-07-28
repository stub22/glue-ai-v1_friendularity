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
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.math.Quaternion;

class VirtualParamFactories {
}

abstract class AODCompatFactory[ResultType] extends Factory[ResultType] {
	def	copyFromAOD(source : ArrayOfDoubles, target: ResultType)
}

class Vec3fFactory extends AODCompatFactory[Vector3f] {
	override def makeOne() : Vector3f = {
		new Vector3f()
	}
	override def makeArray(size : Int) : Array[Vector3f] = {
		new Array[Vector3f](size)
	}
	override def shallowCopyContents(source : Vector3f, target: Vector3f) {
		target.set(source)
	}
	override def	copyFromAOD(source : ArrayOfDoubles, target: Vector3f) {
		
	}
}

class QuaternFactory extends AODCompatFactory[Quaternion] {
	override def makeOne() : Quaternion = {
		new Quaternion()
	}
	override def makeArray(size : Int) : Array[Quaternion] = {
		new Array[Quaternion](size)
	}
	override def shallowCopyContents(source : Quaternion, target: Quaternion) {
		target.set(source)
	}
	override def	copyFromAOD(source : ArrayOfDoubles, target: Quaternion) {
		
	}
	
}
class ColorFactory extends AODCompatFactory[ColorRGBA] {
	override def makeOne() : ColorRGBA = {
		new ColorRGBA()
	}
	override def makeArray(size : Int) : Array[ColorRGBA] = {
		new Array[ColorRGBA](size)
	}
	override def shallowCopyContents(source : ColorRGBA, target: ColorRGBA) {
		target.set(source)
	}
	override def	copyFromAOD(source : ArrayOfDoubles, target: ColorRGBA) {
		
	}	
}