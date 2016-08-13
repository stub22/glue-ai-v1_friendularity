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

import akka.actor.Actor
import com.jme3.math.{Vector2f, Vector3f}
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.Node
import com.jme3.scene.shape.Quad
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.field.{RemoteItemField, VWTestFieldIdents, BoundToDataSrc}


// import org.friendularity.navui.NavPage_Bodies
import org.friendularity.vwmsg.{VWSetupOvlBookRq, VWStageOpticsBasic, NavCmd, InnerNavCmds}

import scala.collection.mutable.{ListBuffer, HashMap => MutableHashMap}
import com.jme3.scene.{Node => JmeNode, Mesh}


/**
  * Created by Stub22 on 7/13/2016.
  */

// NavPages are 2D depictions, usually using some transparency, usually attached somewhere under guiNode.
// They may be navigated by user using the NavLogic keybindings.
trait  OverlayPage extends IdentHlp {
	def getPageID : Ident = makeStampyRandyIdentAnon()

	protected def makeTopFlatWidget : FlatWidget
	lazy val myTopWidget : FlatWidget = makeTopFlatWidget
	def getTopFlatWidget : FlatWidget = myTopWidget

	private var mySvcGate : SvcGate = null
	def getSvcGate : SvcGate = mySvcGate
	def setSvcGate(sg : SvcGate) : Unit = {
		mySvcGate = sg
	}
}
trait BoundPage {
	def lookupBinding(g : FlatGadget) : Option[BoundToDataSrc]
}
trait  GadgetMaker extends IdentHlp {
	def makeFixedTxtLine(gadgID : Ident, txt : String) : TextLine = new FixedFlatGadgetTxtLine(gadgID, txt)
	def makeUpdatableTxtLine(dataSrcID : Ident, preferredSize : Int) : UpdatingTextLine = {
		//		val remote = ???
		val sizeHints =  new OneLineTxtBlkSzHints(Some(preferredSize / 2), Some(preferredSize), Some(preferredSize * 2))
		val gadg = new UpdatingTextLineGadget(makeStampyRandyIdentAnon(), sizeHints)
		gadg
	}
}
/*
trait PageItem {
	protected def getGadget = ???
	protected def getPartnerItem : RemoteItemField[_]
}
*/
// trait Clctn extends PageItem // Collection of items, possibly ordered
// trait Assembly extends PageItem // Has summary and also subcontent, which may be shown or hidden
// trait Leaf extends PageItem // No subcontent

trait JmeNodeSupplier {
	def getNode : JmeNode
}
case class HasJmeNode(myNode : JmeNode) extends JmeNodeSupplier {
	override def getNode : JmeNode = myNode
}
case class MakesJmeNode(myNodeName : String) extends JmeNodeSupplier {
	lazy val myNode : JmeNode = new JmeNode(myNodeName)
	override def getNode : JmeNode = myNode
}

trait OvlDisplayHelp extends SvcGate with SpatMatHelper with VarargsLogging  {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(getRRC)
	val myBrushJar = new BrushJar(myMatPal)

	val pixPerUnitMeshX = 5
	val pixPerUnitMeshY = 5
	// create a single small quad, reusable in many geoms, which can each be scaled
	val myUnitQuadMesh: Mesh = new Quad(pixPerUnitMeshX,pixPerUnitMeshY) // Arbitrary size decision, means 5-pix x 5-pix *if* in 2D GUI.

	val redQuadMaker = new MeshGeoMaker(myUnitQuadMesh, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(myUnitQuadMesh, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)

	def layoutGadgSpatsInLine(parentSupplier : JmeNodeSupplier, gadgs : List[FlatGadget], initPos : Vector3f,
							  incr : Vector3f) : JmeNode = {
		val parentJmeNode = parentSupplier.getNode
		val curr = initPos.clone()
		for (g <- gadgs) {
			val gadgSpat = g.getSpat(this)
			gadgSpat.setLocalTranslation(curr)
			parentJmeNode.attachChild(gadgSpat)
			curr.addLocal(incr)
		}
		parentJmeNode
	}


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

		// Currently assuming 640x480 window, with 30 pix margins.
		// JME 2D objects (unlike 3D objects) are measured in pixels.
		val expectScreenWidthPix = 640 // TODO:  Allow replacement with actual width
		val ovlWidthFrac : Float = 580.0f / 640.0f
		val hgWidthPix : Float = ovlWidthFrac * expectScreenWidthPix
		val hgWidthScale = hgWidthPix / pixPerUnitMeshX

		val expectScreenHeightPix = 480 // TODO: Allow replacement with actual screen height
		val ovlHeightFrac : Float = 420.0f / 480.0f
		val hgHeightPix : Float = ovlHeightFrac * expectScreenHeightPix
		val hgHeightScale = hgHeightPix / pixPerUnitMeshY

		highGeom.setLocalScale(hgWidthScale, hgHeightScale, 1.0f)  // For 5X5 mesh, gives dim=550, 400
		highFlatNode.attachChild(highGeom)

		val xLeftMarginPix : Float = expectScreenWidthPix * (1.0f - ovlWidthFrac) / 2.0f
		val yBottomMarginPix : Float = expectScreenHeightPix * (1.0f - ovlHeightFrac) / 2.0f

		val hnTrnslt = new Vector3f(xLeftMarginPix, yBottomMarginPix, 0.9f)
		highFlatNode.setLocalTranslation(hnTrnslt)
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
	def newWay: JmeNode = {
		val gadgs = getPageChooserGadgetsInDisplayOrder
		val selLineParentNode = layoutGadgSpatsInLine(MakesJmeNode("selGadgLine"), gadgs,
					new Vector3f(35.0f, 45.0f, 3.5f), new Vector3f(80.0f, 5.0f, -1.7f))
		val outerFrameNode = makeOuterBookFrameNode
		outerFrameNode.attachChild(selLineParentNode)
		outerFrameNode

	}
	lazy val myOuterFrameNode : JmeNode = {
		newWay
	}
	/*
	def oldWay : JmeNode = {
		val outerFrameNode = makeOuterBookFrameNode
		val selectorGroupNode = new JmeNode("selectorGadgets")
		val yOff = 40.0f
		var xOff = 10.0f
		for (pcg <- getPageChooserGadgetsInDisplayOrder) {
			val gadgSpat = pcg.getSpat(this)
			gadgSpat.setLocalTranslation(xOff, yOff, 0.3f)
			selectorGroupNode.attachChild(gadgSpat)
			xOff += 80.0f
		}
		outerFrameNode.attachChild(selectorGroupNode)
		outerFrameNode
	}
*/
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
class VWOverlayActor(myRRC : RenderRegistryClient) extends Actor with OverlayLogic {
	override def receive: Actor.Receive = {
		case setupOvl: VWSetupOvlBookRq => {
			setupBook(myRRC, setupOvl.pages)
		}
		case navCmd: NavCmd => {
			processNavCmd(navCmd)
		}
	}
}

trait ValueAcceptor {

}
trait DynamicDisplayPart extends IdentHlp {
	def updateFromSrc(src : CoolDataSrc) : Unit

	def getGadgetForSubfield(subFieldID : Ident) : UpdatingTextLineGadget = ???

	def layoutSpatialTree(dispHlp : OvlDisplayHelp, flagParentIsVertical : Boolean,
						  groupSizeHints : TextBlockSizeParams) : JmeNode = ???
}
trait CoolDataSrc {
	def getDataVector3f(fieldID : Ident) : Vector3f
	def getDataVector2f(fieldID : Ident) : Vector2f
}
case class FloatDataGadget(id : Ident, sizeHints : TextBlockSizeParams)
			extends UpdatingTextLineGadget(id, sizeHints)
case class DDPartVector3f(label : String, fieldID : Ident) extends DynamicDisplayPart {

	val fieldSizeHints = new OneLineTxtBlkSzHints(Some(3), Some(7), Some(11))
	val xGadget = new FloatDataGadget(makeStampyRandyIdentAnon(), fieldSizeHints)
	val yGadget = new FloatDataGadget(makeStampyRandyIdentAnon(), fieldSizeHints)
	val zGadget = new FloatDataGadget(makeStampyRandyIdentAnon(), fieldSizeHints)

	def layoutVert(dispHlp : OvlDisplayHelp) : JmeNode = {
		dispHlp.layoutGadgSpatsInLine(MakesJmeNode("nodeFor_" + fieldID.getLocalName),
					List(xGadget, yGadget, zGadget),
					new Vector3f(), new Vector3f())
	}

	def updateDisplayValues(v3f : Vector3f) = {
		val xGadget = getGadgetForSubfield(VWTestFieldIdents.PROP_hasX)
		val yGadget = getGadgetForSubfield(VWTestFieldIdents.PROP_hasY)
		xGadget.updateTextLine(v3f.getX.toString)
		yGadget.updateTextLine(v3f.getY.toString)

	}
	override def updateFromSrc(src : CoolDataSrc) : Unit = {
		val v3f = src.getDataVector3f(fieldID)
		updateDisplayValues(v3f)
	}


}
case class DDPartVector2f() extends DynamicDisplayPart {
	def updateDisplayValue(v2f : Vector2f) = ???
	override def updateFromSrc(src : CoolDataSrc) : Unit = ???
}
trait DynamicDisplayEntity {
	def addPartDisplay(partFieldID : Ident, ddPart : DynamicDisplayPart) = ???
	def updateDisplayedText(partFieldID : Ident, fragFieldID : Option[Ident], upTxt : String) = ???
	def findDisplayPart(partID : Ident) : DynamicDisplayPart = ???
	def getAllParts : Traversable[DynamicDisplayPart] = Nil
	def updateAllParts(dataSrc : CoolDataSrc) : Unit = {
		val allParts = getAllParts
		for (p <- allParts) {
			p.updateFromSrc(dataSrc)
		}
	}

}
trait CamStatusDisplayLogicGoesWhereHuh {
	val myCamSummariesByID = new  MutableHashMap[Ident, VWCamSummary]()
	val myDisplayEntitiesByCamID = Map()
	def absorbCamStatusSummary(camStatSum : VWCamSummary) : Unit = {
		// Here we are receiving a single large leaf-field data chunk.  It is a composite, but not of items/fields.
		// Two extremes in how we can think about processing it.
		// field-Factored way:  We map into set of smaller field updates that go directly to gadgets for display.
		// page-Pulled way:  Write one comprehensive display updater, bypass gadgets, render in one swoop.

		// Former is a formal block-construction approach, easy to add properties, config-compatible, authoring-oriented.
		// Pages will display uniformly, display of all of them is improved centrally.

		// Latter allows for more customization at page level, is more code-oriented, easier to improve individual
		// page layout.  Pages will diverge in appearance and behavior until refactored by libs.

	}
	def setupNewDisplayEntity(camID : Ident) : Unit = {
		val posDDPart = DDPartVector3f("Position", VWTestFieldIdents.PROP_hasPos)
		val pointDDPart = DDPartVector3f("Point Dir", VWTestFieldIdents.PROP_hasPointDir)
		val ddEntity = new DynamicDisplayEntity{}
		ddEntity.addPartDisplay(posDDPart.fieldID, posDDPart)
		ddEntity.addPartDisplay(pointDDPart.fieldID, pointDDPart)

	}
	def copySummaryValuesToTargets(camSum : VWCamSummary, ddEntity : DynamicDisplayEntity) : Unit = {
		val posPart = ddEntity.findDisplayPart(VWTestFieldIdents.PROP_hasPos).asInstanceOf[DDPartVector3f]
		posPart.updateDisplayValues(camSum.posLoc)
		val pointDirPart = ddEntity.findDisplayPart(VWTestFieldIdents.PROP_hasPointDir).asInstanceOf[DDPartVector3f]
	}

}