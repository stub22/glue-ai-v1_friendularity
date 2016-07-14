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

package org.friendularity.vwimpl

import com.jme3.math.Vector3f
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.Node
import com.jme3.scene.shape.Quad
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.dull.RemoteItemField
import org.friendularity.navui.NavPage_Bodies
import org.friendularity.vwmsg.{NavCmd, InnerNavCmds}

import scala.collection.mutable.{ListBuffer, HashMap => MutableHashMap}
import com.jme3.scene.{Node => JmeNode, Mesh}


/**
  * Created by Stub22 on 7/13/2016.
  */

// NavPages are 2D depictions, usually using some transparency, usually attached somewhere under guiNode.
// They may be navigated by user using the NavLogic keybindings.
trait  OverlayPage extends IdentHlp {
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

trait OvlDisplayHelp extends SvcGate with SpatMatHelper with VarargsLogging  {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(getRRC)
	val myBrushJar = new BrushJar(myMatPal)
	val quadMeshFiveByFive: Mesh = new Quad(5,5)

	val redQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)

}
trait OvlOuterDisplayHelp extends OvlDisplayHelp {
	def makeOuterBookFrameNode(): JmeNode = {
		// We expect this flatNode to be added very high up in the guiNode tree, possibly as child of root.
		val highFlatNode = new JmeNode("navBookTopNode")
		// Big fixed rectangle to serve as parent+background of all the other rectangles, should have
		// fairly low opacity.
		val highGeom = orngQuadMaker.makeGeom("orangeQuadFixMe")
		val guiBucket: RenderQueue.Bucket = RenderQueue.Bucket.Gui
		highGeom.setQueueBucket(guiBucket)
		highGeom.setLocalScale(40.0f, 40.0f, 1.0f)
		highFlatNode.attachChild(highGeom)
		highFlatNode.setLocalTranslation(new Vector3f(150.0f, 120.0f, 0.9f))
		highFlatNode
	}
}
class OvlPageBook(myRRC : RenderRegistryClient) extends OvlOuterDisplayHelp {
	private val myPagesByID = new MutableHashMap[Ident, OverlayPage]()
	private val myPageDisplayOrderBuf = new ListBuffer[Ident]
	private var myActivePage : Option[OverlayPage] = None

	def registerPage(np : OverlayPage): Unit = {
		val id = np.getPageID
		myPageDisplayOrderBuf.append(id)
		myPagesByID.put(id, np)
		np.setSvcGate(this)
	}
	private def getPagesInDisplayOrder : List[OverlayPage] = myPageDisplayOrderBuf.toList.flatMap(myPagesByID.get(_))
	def getPageChooserGadgetsInDisplayOrder : List[FlatGadget] = getPagesInDisplayOrder.map(_.getTopFlatWidget.getOuterGadgetForMe)

	lazy val myOuterFrameNode : JmeNode = {
		val outerFrameNode = makeOuterBookFrameNode
		val selectorGroupNode = new JmeNode("selectorGadgets")
		val yOff = 40.0f
		var xOff = 10.0f
		for (pcg <- getPageChooserGadgetsInDisplayOrder) {
			val gadgSpat = pcg.getSpat(this)
			gadgSpat.setLocalTranslation(xOff, yOff, 0.3f)
			selectorGroupNode.attachChild(gadgSpat)
			xOff += 40.0f
		}
		outerFrameNode.attachChild(selectorGroupNode)
		outerFrameNode
	}

	var isShown : Boolean = false
	def showOrHideBook(showIt : Boolean): Unit = {
		val bookNode = myOuterFrameNode
		val parentNode = getRRC.getJme3RootOverlayNode(null)
		if (showIt) {
			info1("Showing bookNode: {}", bookNode)
			enqueueAttach(bookNode, parentNode)
		} else {
			info1("Hiding bookNode: {}", bookNode)
			enqueueDetach(bookNode)
		}
		isShown = showIt
	}

	def switchActivePage(nxtPgID : Ident) : Unit = {
	}

	override protected def getTooMuchRRC: RenderRegistryClient = myRRC

	override protected def getRRC: RenderRegistryClient = myRRC
}

trait OverlayLogic extends VarargsLogging with InnerNavCmds  {
	var myBook_opt : Option[OvlPageBook] = None
	def setupBook(rrc : RenderRegistryClient, pages : List[OverlayPage]): Unit = {
		val book = new OvlPageBook(rrc)
		pages.map(book.registerPage(_))
		myBook_opt = Option(book)

	}
	def toggleBookDisplay(): Unit = {
		if (myBook_opt.isDefined) {
			val book = myBook_opt.get
			val isDisplayed = book.isShown
			val oppositeState : Boolean = !isDisplayed
			book.showOrHideBook(oppositeState)
		} else error0("Cannot toggle overlay-book display, no book is initialized")
	}
	def processNavCmd(nc : NavCmd): Unit = {
		nc match {
			case NCmd_SHOW_TOGGLE => {
				info1("Processing SHOW TOGGLE cmd: {}", nc)
				toggleBookDisplay()
//				val bodyPage = new NavPage_Bodies()
//				val topWidg = bodyPage.getTopFlatWidget
//				info1("Got bodies topFlatWidget={}", topWidg)
			}
			case NCmd_GO_IN => {
				info1("Processing GO IN cmd: {}", nc)
			}
			case NCmd_GO_OUT => {
				info1("Processing GO OUT cmd: {}", nc)
			}
			case _ => {
				info1("Processing unexpected cmd: {}", nc)
			}
		}
	}
}
