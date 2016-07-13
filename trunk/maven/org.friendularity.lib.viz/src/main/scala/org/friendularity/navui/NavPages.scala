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
import org.friendularity.vwimpl.{SvcGate, AttachHlp, FullEnqHlp, TextSpatMaker, MeshGeoMaker, BrushJar, MatPallete, TextLine, UpdatingTextLineGadget, UpdatingTextLine, IdentHlp, FixedFlatGadgetTxtLine, FlatGadget, FinishedFlatWidget, FlatWidget}
import com.jme3.scene.{Node => JmeNode, Mesh}

import scala.collection.mutable.ListBuffer

/**
  * Created by Stub22 on 7/7/2016.
  */


	// NavPages are 2D depictions, usually using some transparency, usually attached somewhere under guiNode.
// They may be navigated by user using the NavLogic keybindings.
trait  NavPage extends IdentHlp {
	def getPageID : Ident = makeStampyRandyIdent()
	protected def makeTopFlatWidget : FlatWidget
	lazy val myTopWidget : FlatWidget = makeTopFlatWidget
	def getTopFlatWidget : FlatWidget = myTopWidget

	private var mySvcGate : SvcGate = null
	def getSvcGate : SvcGate = mySvcGate
	def setSvcGate(sg : SvcGate) : Unit = {
		mySvcGate = sg
	}
}
trait  GadgetMaker extends IdentHlp {
	def makeFixedTxtLine(txt : String) : TextLine = new FixedFlatGadgetTxtLine(makeStampyRandyIdent(), txt)
	def makeUpdatableTxtLine(dataSrcID : Ident) : UpdatingTextLine = {
//		val remote = ???
		val gadg = new UpdatingTextLineGadget(makeStampyRandyIdent())
		gadg
	}
}
trait PageItem {
	protected def getGadget = ???
	protected def getPartnerItem : RemoteItemField[_]
}
trait Clctn extends PageItem // Collection of items, possibly ordered
trait Assembly extends PageItem // Has summary and also subcontent, which may be shown or hidden
trait Leaf extends PageItem // No subcontent


// We expect VW-picking to eventually interact sensibly with any of these.
class NavPage_Bodies extends NavPage with  GadgetMaker  {
	val b1DataSrcID = makeStampyRandyIdent()
	val b2DataSrcID = makeStampyRandyIdent()

	// Table showing gross properties of all bodies, plus we see detail on selected body col/row.
	val myFieldGadgs : List[FlatGadget] = List(
				makeFixedTxtLine("body-one"), makeUpdatableTxtLine(b1DataSrcID),
				makeFixedTxtLine("body-two"), makeUpdatableTxtLine(b2DataSrcID)).map(_.asInstanceOf[FlatGadget])
	val myOuterGadg : FlatGadget = makeFixedTxtLine("bodies-outer").asInstanceOf[FlatGadget]

	override def makeTopFlatWidget: FlatWidget =  new FinishedFlatWidget(myFieldGadgs, myOuterGadg)

}
trait NavPage_Anims extends NavPage {
	// Shows available anims of each kind, highlighting any that are running, and allowing
	// for triggering and cancelling.
}
trait NavPage_Cams extends NavPage {
	// Table showing properties of each cam, mainly pos+dir.  Ops to manipulate cams are lower priority.
	override def makeTopFlatWidget: FlatWidget = ???
}
trait NavPage_Goodies extends NavPage {
	// Shows available goodies of each kind.  Highlight row to select the actual goody in (2 or) 3-space.
	// Also shows stats for goody messages rcvd, total and per-kind and per-goody.
	// Blinks colors on new msg.
	override def makeTopFlatWidget: FlatWidget = ???
}
trait NavPage_Speech extends NavPage {
	// Shows any running speech job, plus some stats.
	// Should also show bookmark info and viseme summary.
	// Eventually can lead into speech config menus.
}
// Additional pages to come:
trait NavPage_Modulators extends NavPage  	// = Content players and behavioral state machines
trait NavPage_Sys extends NavPage 			// = Status of JVM, network connections
trait NavPage_Sensors extends NavPage 		// = Estim features = the salient point of our structure

import scala.collection.mutable.{HashMap => MutableHashMap}

class NavBook (myRRC : RenderRegistryClient, myMatPal : MatPallete) extends SvcGate {
	override protected def getRRC : RenderRegistryClient = myRRC
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(myRRC)
	val myBrushJar = new BrushJar(myMatPal)

	val quadMeshFiveByFive: Mesh = new Quad(5,5)

	val redQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)

	private val myPagesByID = new MutableHashMap[Ident, NavPage]()
	private val myPageDisplayOrderBuf = new ListBuffer[Ident]
	private var myActivePage : Option[NavPage] = None

	private def registerPage(np : NavPage): Unit = {
		val id = np.getPageID
		myPageDisplayOrderBuf.append(id)
		myPagesByID.put(id, np)
		np.setSvcGate(this)
	}
	registerPage(new NavPage_Bodies{})
	registerPage(new NavPage_Cams{})
	registerPage(new NavPage_Goodies{})

	lazy val myOuterFrameNode : JmeNode = makeOuterBookFrameNode

	def showOrHideBook(showIt : Boolean): Unit = {
		val bookNode = myOuterFrameNode
		val parentNode = myRRC.getJme3RootOverlayNode(null)
		if (showIt) {
			enqueueAttach(bookNode, parentNode)
		} else {
			enqueueDetach(bookNode)
		}
	}

	private def getPagesInDisplayOrder : List[NavPage] = myPageDisplayOrderBuf.toList.flatMap(myPagesByID.get(_))
	def getPageChooserGadgetsInDisplayOrder : List[FlatGadget] = getPagesInDisplayOrder.map(_.getTopFlatWidget.getOuterGadgetForMe)
	def makeOuterBookFrameNode(): JmeNode = {
		// We expect this flatNode to be added very high up in the guiNode tree, possibly as child of root.
		val highFlatNode = new JmeNode("navBookTopNode")
		// Big fixed rectangle to serve as parent+background of all the other rectangles, should have
		// fairly low opacity.
		val highGeom = orngQuadMaker.makeGeom("orangeQuadFixMe")
		highGeom.scale(100.0f)
		highFlatNode.attachChild(highGeom)
		highFlatNode
	}
	def switchActivePage(nxtPgID : Ident) : Unit = {

	}

}