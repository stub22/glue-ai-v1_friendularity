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
package org.friendularity.vw.impl.shp

import com.jme3.scene.{Node => JmeNode, Spatial}
import org.appdapter.core.name.Ident
import org.friendularity.vw.impl.cam.SyncsToCam
import org.friendularity.vw.impl.manip.Manipable
import org.friendularity.vw.msg.shp.deep.{VWSCR_CamGuideNode, VWShapeCreateRq}

/**
  * Code moved to new file on 1/19/2017.
  */
trait SyncMe {
	// Does nothing by default, but may be overidden, e.g. by CamGuideeMSR below.
	def syncAfterAttach_onRendThrd : Unit = { }
}

// Because this class is manipable, the spatial may be smooved or abruptlyMoved.
abstract class MadeSpatRecBase extends Manipable with SyncMe

case class MadeSpatRec(mySpat : Spatial, myID_opt : Option[Ident], myCreateRq : VWShapeCreateRq)
			extends MadeSpatRecBase() {
	override def getMainSpat : Spatial = mySpat
	override def getID : Ident = myID_opt.get // Will throw if myID_opt is None!

}
case class CamGuideMadeSpatRec(myGuideNode : JmeNode, myGuideID : Ident, myCreateRq : VWSCR_CamGuideNode)
			extends MadeSpatRecBase() with SyncsToCam {
	override def getMainSpat : Spatial = myGuideNode
	override def getID : Ident = myGuideID

	override def syncAfterAttach_onRendThrd : Unit = {
		syncGuideToCam_rendThrd
	}
}
