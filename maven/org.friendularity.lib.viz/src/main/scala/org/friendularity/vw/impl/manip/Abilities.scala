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

import com.jme3.math.ColorRGBA
import com.jme3.scene.Spatial
import org.appdapter.core.name.Ident
import org.friendularity.vw.mprt.manip.{TransformParams3D, Transform3D}

/**
  * Code moved to new file on 1/19/2017.
  */
trait HasMainSpat {
	def getMainSpat : Spatial
}
trait HasMainGeom {

}
trait Pickable { // Picked = Selected by user through some UI.
def notifyPickedState_mystThrd(isPicked : Boolean) : Unit // Notice allows us to display specially
}
trait Addressable {
	def getID : Ident
}
trait Colorable extends HasMainGeom {
	def applyColor_rendThrd(crgba : ColorRGBA)
}
trait Locatable extends HasMainSpat {
	// Reads but doesn't disturb VW scene graph, so OK on any thrd.
	def getCurrXform_anyThrd: Transform3D = {
		val spat = getMainSpat
		val locXform = spat.getLocalTransform
		val locPos = locXform.getTranslation
		val locRot = locXform.getRotation
		val locScl = locXform.getScale

		val xform3D = new TransformParams3D(locPos, locRot, locScl)
		xform3D
	}

}
