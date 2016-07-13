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
package org.friendularity.navui

import com.jme3.scene.shape.Quad
import org.appdapter.core.name.{FreeIdent, Ident}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.dull.RemoteItemField
import org.friendularity.vwimpl.{OvlPageBook, GadgetMaker, OverlayPage, SvcGate, AttachHlp, FullEnqHlp, TextSpatMaker, MeshGeoMaker, BrushJar, MatPallete, TextLine, UpdatingTextLineGadget, UpdatingTextLine, IdentHlp, FixedFlatGadgetTxtLine, FlatGadget, FinishedFlatWidget, FlatWidget}
import com.jme3.scene.{Node => JmeNode, Mesh}

import scala.collection.mutable.ListBuffer

/**
  * Created by Stub22 on 7/7/2016.
  */



// We expect VW-picking to eventually interact sensibly with any of these.
class NavPage_Bodies extends OverlayPage with  GadgetMaker  {
	val b1DataSrcID = makeStampyRandyIdent()
	val b2DataSrcID = makeStampyRandyIdent()

	// Table showing gross properties of all bodies, plus we see detail on selected body col/row.
	val myFieldGadgs : List[FlatGadget] = List(
				makeFixedTxtLine("body-one"), makeUpdatableTxtLine(b1DataSrcID),
				makeFixedTxtLine("body-two"), makeUpdatableTxtLine(b2DataSrcID)).map(_.asInstanceOf[FlatGadget])
	val myOuterGadg : FlatGadget = makeFixedTxtLine("bodies-outer").asInstanceOf[FlatGadget]

	override def makeTopFlatWidget: FlatWidget =  new FinishedFlatWidget(myFieldGadgs, myOuterGadg)

}
trait NavPage_Anims extends OverlayPage {
	// Shows available anims of each kind, highlighting any that are running, and allowing
	// for triggering and cancelling.
}
trait NavPage_Cams extends OverlayPage {
	// Table showing properties of each cam, mainly pos+dir.  Ops to manipulate cams are lower priority.
	override def makeTopFlatWidget: FlatWidget = ???
}
trait NavPage_Goodies extends OverlayPage {
	// Shows available goodies of each kind.  Highlight row to select the actual goody in (2 or) 3-space.
	// Also shows stats for goody messages rcvd, total and per-kind and per-goody.
	// Blinks colors on new msg.
	override def makeTopFlatWidget: FlatWidget = ???
}
trait NavPage_Speech extends OverlayPage {
	// Shows any running speech job, plus some stats.
	// Should also show bookmark info and viseme summary.
	// Eventually can lead into speech config menus.
}
// Additional pages to come:
trait NavPage_Modulators extends OverlayPage  	// = Content players and behavioral state machines
trait NavPage_Sys extends OverlayPage 			// = Status of JVM, network connections
trait NavPage_Sensors extends OverlayPage 		// = Estim features = the salient point of our structure

import scala.collection.mutable.{HashMap => MutableHashMap}

class NavBook (myRRC : RenderRegistryClient, myMatPal : MatPallete) extends OvlPageBook(myRRC) {
	override protected def getRRC : RenderRegistryClient = myRRC

	registerPage(new NavPage_Bodies{})
	registerPage(new NavPage_Cams{})
	registerPage(new NavPage_Goodies{})



}