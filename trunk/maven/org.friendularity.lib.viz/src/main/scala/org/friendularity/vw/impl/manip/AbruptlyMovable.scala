/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.vw.impl.manip

import com.jme3.math.Vector3f
import com.jme3.scene.Spatial
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vw.mprt.manip.{Transform3D, MaybeTransform3D}
import org.friendularity.vw.msg.shp.deep.DoTransformAbsoluteNow

/**
  * Code moved to new file on 1/19/2017.
  *
  * partial -> apply only those properties supplied, leave others unmodified in tgt
  * full -> apply all properties, using defaults where needed
  */
trait AppliesXforms extends VarargsLogging {
	// full -> set all properties in tgt, using defaults as needed.
	def appliesTransform_full_runThrd(tgtSpat : Spatial, xform : Transform3D) : Unit = {
		// These "get" ops will generally return a default if no specific value was given.
		// That defaulting is what makes this a "full" transform apply.
		val fPos = xform.getPos
		val fRotQuat = xform.getRotQuat
		val fScale = xform.getScale
		tgtSpat.setLocalTranslation(fPos)
		tgtSpat.setLocalRotation(fRotQuat)
		tgtSpat.setLocalScale(fScale)
	}
	// partial -> set only properties that are supplied
	def applyTransform_partial_runThrd(tgtSpat : Spatial, xform : MaybeTransform3D) : Unit = {
		info2("Applying transform on run thread, spat={}, xform={}", tgtSpat, xform)
		val fPos_opt = xform.getPos_opt
		val fRotQuat_opt = xform.getRotQuat_opt
		val fScale_opt = xform.getScl_opt
		fPos_opt.map(tgtSpat.setLocalTranslation(_))
		fRotQuat_opt.map(tgtSpat.setLocalRotation(_))
		fScale_opt.map(tgtSpat.setLocalScale(_))
	}

}
trait AbruptlyMovable extends HasMainSpat with AppliesXforms {

	def applyTransform_partial_runThrd(xform : MaybeTransform3D) : Unit = {
		// This impl does not worry about any existing animCtrl.
		// Override (in Smoovable or other) to protect from animCtrls, or to use them.
		val tgtSpat = getMainSpat
		applyTransform_partial_runThrd(tgtSpat, xform)
	}
	def applyTransform_full_runThrd(xform : Transform3D) : Unit = {
		// This impl does not worry about any existing animCtrl.
		// Override (in Smoovable or other) to protect from animCtrls, or to use them.
		val tgtSpat = getMainSpat
		appliesTransform_full_runThrd(tgtSpat, xform)
	}

	private def rotToLookAtWorldPos_UNUSED(dirToLook_worldCoord : Vector3f, upDir : Vector3f) : Unit = {
		// Fun JME shortcut which we don't currently use, but want to keep in mind at this level.
		// From the source of Spatial.java
		// " Unlike {@link Quaternion#lookAt(com.jme3.math.Vector3f, com.jme3.math.Vector3f) }
		// * this method takes a world position to look at and not a relative direction.
		//
		getMainSpat.lookAt(dirToLook_worldCoord, upDir)
	}

	private def UNUSED_applyTransformAbs_runThrd(xformAbs : DoTransformAbsoluteNow) : Unit = {
		applyTransform_partial_runThrd(xformAbs.getAbsXform)
	}
}
