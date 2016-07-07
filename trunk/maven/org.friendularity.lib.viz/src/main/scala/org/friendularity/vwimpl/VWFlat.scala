package org.friendularity.vwimpl

import com.jme3.scene.{Node => JmeNode}
import org.appdapter.core.name.{FreeIdent, Ident}

import scala.collection.mutable

/**
  * Created by Stub22 on 7/6/2016.
  */
trait VWFlatLogic {
	def makeHidableNavScreen2D(): JmeNode = {
		// Request a semi-opaque nav-screen which includes our key-menu help, opacity adjust,
		// and obj-property inspector.   "Picking" in 3-D space selects object to show props.
		// Bone states have a screen  its own screen.  Anim-state and speech-out blocks are
		// viewable as 2D text and numbers, bar-graphs, dials, etc.
		// Up to 1 item may be selected within a particular screen, and when that screen has focus,
		// that selected item is the target of commands from keyboard.  When this screen is up,
		// it should always be clear
		// to user that exactly 1 thing is selected and now-receiving commands.
		// More generally, ongoing funcs of time and state are displayed in nesting, paged,
		// rectangular 2-scenes.

		// Impl is primarily organized around text, simple 2-D shapes esp. rectangles, and color incl. opacity.
		// "Shape" space may be considered the 3-deep cousin of this FlatSpace.
		// Objects in both are keyed by Ident, and interfacable to ThingActs.
		null
	}

}
object FunFlatGadgetKinds {
	// Can be displayed, can update values and notify in both directions
	val FGK_textLine: Ident = new FreeIdent("urn:FGK_textLine#id")
	val FGK_chooseInt: Ident = new FreeIdent("urn:FGK_chooseInt#id")
	val FGK_chooseItem: Ident = new FreeIdent("urn:FGK_chooseItem#id")
}

// Concepts are  Gadget, Widget, DisplayField

// Gadget is a field-handle for state progation.  Can be Subclassed for different data prop shapes+assumptions.
trait FlatGadget {
	def getID : Ident
	def getKindID : Ident
}
class FlatGadgetImpl(myID: Ident, kindID: Ident) extends FlatGadget {
	override def getID : Ident = myID
	override def getKindID : Ident = kindID
}


case class FlatGadgetTxtLine(id: Ident) extends FlatGadgetImpl(id, FunFlatGadgetKinds.FGK_textLine)

// Subclass to customize the behavior of a group of displayed fields
trait FlatWidget {
	def getFieldGadgets : List[FlatGadget]
	def insertFieldGadget(fg : FlatGadget): Unit
}

// Subclass to give different field-level UI patterns
trait FlatFieldDisplay {
	def getBoundGadget : FlatGadget = ???
	def updateFromGadget : Unit = ??? // Reads latest data known by Gadget, send to UI
	def recordUserChange : Unit = ??? // Pull data from UI, send to Gadget

	def makeSpatialTree(layoutAssumps : AnyRef) = ???
}

class FlatFieldDisplayImpl extends FlatFieldDisplay {

}
class FlatWidgetImpl extends FlatWidget {
	val fieldsByGadgetID = new mutable.HashMap[Ident, FlatFieldDisplay]()

	override def getFieldGadgets : List[FlatGadget] = {
		fieldsByGadgetID.values.map(_.getBoundGadget).toList
	}
	override def insertFieldGadget(fg : FlatGadget): Unit = {
		val fieldDisp = new FlatFieldDisplayImpl
		fieldsByGadgetID.put(fg.getID, fieldDisp)
	}
}

class FWAnimStatus extends FlatWidgetImpl {

}
class FWBodyStatus extends FlatWidgetImpl
trait VWCamStatus
trait VWSpeechStatus
trait VWSensorStatus
