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


import com.jme3.asset.AssetManager
import com.jme3.math.{ColorRGBA}
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer
import org.cogchar.render.trial.TextBox2D
import org.friendularity.cpump.{CPStrongTeller, CPumpMsg, CPMsgTeller}
import org.friendularity.field.ItemFieldSpec

// import scala.collection.mutable

/**
  * Created by Stub22 on 7/6/2016.
  *
  * Model of screen definition:
  * 	Widgets are containers
  * 	Gadgets are items found in Widgets, which may activate other Widgets
  *
  * UI interprets these Widget+Gadget sets, and applies known user prefs,
  * to produce actual layouts at a particular moment of use.
  *
  */
trait VWFlatLogic {
	def makeHidableNavScreen2D(): JmeNode = {
		// Request a semi-opaque nav-screen which includes our key-menu help, opacity adjust,
		// and obj-property inspector.   "Picking" in 3-D space selects object to show props.
		// Bone states have a monitoring page showing bone/servo positions.  Anim-state and
		// speech-out blocks are viewable as 2D text and numbers, bar-graphs, dials, etc.
		// Up to 1 item may be selected within a particular screen, and when that screen has focus,
		// that selected item is the target of commands from keyboard.  When this screen is up,
		// it should always be clear
		// to user that exactly 1 thing is selected and now-receiving commands.
		// More generally, ongoing funcs of time and state are displayed in nesting, paged,
		// rectangular 2-scenes.

		// Impl is primarily organized around text, simple 2-D shapes esp. rectangles, and
		// color incl. opacity.
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

// Separation of concerns:
//     	1) Existence and kind of displayable concept
//		2) Display params - static (font) and dynamic (color)
//		3) Sizing of display items (using char-size as a foundation).
//		4) Combination, layout, and navigation of display concepts
//		5) Data binding of suh concepts -- to individual values managed by some actor.

// Concepts are  Gadget, Widget, DisplayField

// Gadget is a field-handle for state progation.  Can be Subclassed for different data prop shapes+assumptions.
// Gadget is expected to be smart about type-conversion (param by ID as needed), so we don't have to param
// by CType.

// Denominate widths + heights in *characters*

trait FlatGadget {
	def getGadgetID : Ident
	def getGadgetKindID : Ident

}
trait SizableAsText {
	def getSizeParams : TextBlockSizeParams
}
// Used to monitor detail on a known field of a known item.
trait FullyBoundToItemField {
	def getAddress : ItemFieldSpec
}

trait BoundToFieldOfVariableItem {
	def getFieldID : Ident
}

class FlatGadgetImpl(myID: Ident, kindID: Ident) extends FlatGadget {
	override def getGadgetID : Ident = myID
	override def getGadgetKindID : Ident = kindID
	// Depending on the kind, we may expect additional attribs.
	// override def getSizeParams : TextBlockSizeParams = sizeParams
}

// A gadget serves as a (named) wiring point to some (named, separately) primitve value
// data source/sink, which usually takes the form of a field of an item.

// A Widget is a wired up set of gadgets, to be displayed or hidden onscreen.
// A partial (or complete, for some purpose) rectangular 2D screen recipe,
// which has a name and may be hidden/shown as a unit ("complete") or sub-unit.
// Subclass to customize the behavior of a group of displayed fields.
trait FlatWidget {  // Is not itself a Gadget, but may produce one
	def getFieldGadgets : List[FlatGadget]
	def getOuterGadgetForMe : FlatGadget

	def showOrHide(showit : Boolean): Unit = {

	}
}
trait LayoutRect
trait WidgetViewSpec {
	def layoutRectangles(someGadgs : List[FlatGadget]) : List[LayoutRect]
	def getLabelTextForGadget(ag : FlatGadget) : String
}
trait UnfinishedFlatWidget extends FlatWidget {
	def insertFieldGadget(fg : FlatGadget): Unit
}
case class FinishedFlatWidget(myFieldGadgs : List[FlatGadget], myOuterGadg : FlatGadget) extends FlatWidget {
	override def getFieldGadgets : List[FlatGadget] = myFieldGadgs
	def getOuterGadgetForMe : FlatGadget = myOuterGadg
}
// 1) Low-level JME boxes are of course allowed to collide and overlap with siblings.
// Here the positions and sizes are in *pixels*.
trait PixelRectangleDesc {
	def getWidth : Integer // Represents some expected *actual* pixel width, not a modeled layout width.
	def getHeight : Integer
	def getX : Integer
	def getY : Integer
	def getZorder : Float
}

trait DisplayBox2D {
	def updatePixelRectDesc(prd : PixelRectangleDesc)
}
// 2) Above that level we have more constrained boxes that do not collide or overlap with any siblings.
// Here the positions and sizes can be in *chars* or other units.
// (height + width of the a label-char or edit-char)
// We can cheat on #2 by using grids
trait DispBox2D {
	// Old impl does not really allow change to width+height
	def setCoordsInPixels (xPix: Integer, yPix: Integer, zOrder: Float, width: Integer, height: Integer) : Unit
	def setText_anyThrd(upTxt : String) : Unit
}
object TestWidgs {
	// Possibly zOrder works better if between -1.0 and +1.0f?  (Some old bug Stub22 saw mentioned in a forum)
	def makeOldTextBox2D_rendThrd(rrc: RenderRegistryClient, requiredID: Ident,
								  optInitText: String,
								  optInitTextColor: ColorRGBA, optInitQuadColor: ColorRGBA) : TextBox2D = {

		val tb2D = new TextBox2D(rrc, requiredID, optInitText, ColorRGBA.White, ColorRGBA.Magenta);
		tb2D.setCoordinates(380, 150, -2.5f, 110, 90, Queuer.QueueingStyle.INLINE)
		// This should be called only once, but can be at any point relative to the parameter/content setting calls,
		// which can then be repeated anytime to update the contents (modulo thread concerns).
		val parentNode : JmeNode = ???
		val assetMgr : AssetManager = ???
		tb2D.setupContentsAndAttachToParent(parentNode, rrc, assetMgr)
		tb2D
	}
}

// From the view perspective, each Gadget is a data source+sink.
// The Gadget is gateway to the
// Subclass to give different field-level UI patterns
trait FlatFieldDisplay[FDT] {
	def notifyStateUpdated(udat : FDT) : Unit = ??? // Latest data from sys-side of our gadget, which we send to UI
	def recordUserChange(udat : FDT) : Unit = ??? // Pull data from UI, send to gadget+sys wiring.
	// System confirms change by notifying us with equal data.

	// Good chunk to modularize, because it can always be done off rend thrd.
	def makeSpatialTree(layoutAssumps : AnyRef) : JmeNode = ???
}

class FlatFieldDisplayImpl[FDT] extends FlatFieldDisplay[FDT] {
	def getBoundGadget : FlatGadget = ???
}
class FlatWidgetImpl extends UnfinishedFlatWidget {
	val fieldDispsByGadgetID = new scala.collection.mutable.HashMap[Ident, FlatFieldDisplay[_]]()

	override def getFieldGadgets : List[FlatGadget] = {
		fieldDispsByGadgetID.values.map(_.asInstanceOf[FlatFieldDisplayImpl[_]].getBoundGadget).toList
	}
	override def insertFieldGadget(fg : FlatGadget): Unit = {
		val fieldDisp = new FlatFieldDisplayImpl
		fieldDispsByGadgetID.put(fg.getGadgetID, fieldDisp)
	}

	override def getOuterGadgetForMe : FlatGadget = {
		val outer = ???
		outer
	}
}


