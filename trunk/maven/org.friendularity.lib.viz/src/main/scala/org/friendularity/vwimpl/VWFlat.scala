package org.friendularity.vwimpl

import com.jme3.asset.AssetManager
import com.jme3.math.{Rectangle, ColorRGBA}
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer
import org.cogchar.render.trial.TextBox2D
import org.friendularity.cpump.{CPStrongTeller, CPumpMsg, CPMsgTeller}

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
		// Bone states have a montioring page showing bone/servo positions.  Anim-state and
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
trait SizeBounds
trait FlatGadget {
	def getGadgetID : Ident
	def getGadgetKindID : Ident
}
// Big trick is that we can use these params to help size things that are not really text, too.
trait TextBlockSizeParams {
	// All measurements in presumed ideal chars
	def getMinWidth : Int = 4
	def getMinHeight : Int = 1
	def getMaxHeight : Int = 10
	def getMaxWidth : Int = 80
	def getPreferredWidth : Int = 24
	def getPreferredHeight : Int = 2
}
trait OneLineTextBlockSizeParams extends TextBlockSizeParams {
	override def getMaxHeight : Int = 1
	override def getPreferredHeight : Int = 1
}

class FlatGadgetImpl(myID: Ident, kindID: Ident) extends FlatGadget {
	override def getGadgetID : Ident = myID
	override def getGadgetKindID : Ident = kindID
	// Depending on the kind, we may expect additional attribs.
}

// A gadget serves as a (named) wiring point to some (named, separately) primitve value
// data source/sink, which usually takes the form of a field of an item.

// Item and Field are our primary terms to describe app data structures enabled for change-notice events.
// This structure is highly compatible with RDF, but does not strictly require RDF at any boundary.
// Thus implementations are free to use any RDF system, or none.  Generally slower-changing structural
// data benefits from RDF infrastructure, while high-volume regular data (e.g. video frames) benefits much less.

// Item = Resource
// Field = Property, optionally defined via rdfs: or owl:DataProperty, owl:ObjectProperty.
// An owl:AnnotationProperty does not have a field.
// ItemField = Reference to the value of a property at a resource
// The value can be of two main logical forms:
// 1) "Bag" = Set of item-refs = set of Idents = set of URIs
// (implies ObjectProperty value = Unordered  collection of items (but may be tagged with order meaning on a serialization)
// Bag = Collection of resources, formally equiv. to a set.  O

// 2) "Leaf" = Single leaf value, corresponding to an RDF Literal Value at a Functional owl:DataProperty
// We do not support multi-valued data properties directly; instead such a property must be mapped
// to being a set of synthetic items which each have the corresponding field value.

trait ItemFieldSpec {
	def getItemID : Ident
	def getFieldID : Ident
}

case class ItemFieldSpecDirectImpl(myItemID : Ident, myFieldID : Ident) extends ItemFieldSpec {
	override def getItemID: Ident = myItemID
	override def getFieldID: Ident = myFieldID
}
// Perceived value expression at an ItemFieldSpec is an instance of ItemFieldData.
trait ItemFieldData {
	def getFieldAddress : ItemFieldSpec
}
trait FieldDataLeaf extends ItemFieldData {
	// Single primitive/literal value
	def getData : Any
}
trait TypedFieldDataLeaf[LDT] extends FieldDataLeaf {
	override def getData : Any = getTypedData
	def getTypedData : LDT
}
trait FieldDataItemBag extends ItemFieldData {
	// Unordered collection of Item-IDs, which may be used as further query keys in get___SubFields
	def getContainedItemIDs : Set[Ident]
}
class ShallowFieldDataItemBag(myFieldAddr : ItemFieldSpec, myContItemIDs : Set[Ident]) extends FieldDataItemBag {
	override def getFieldAddress: ItemFieldSpec = myFieldAddr

	override def getContainedItemIDs : Set[Ident] = myContItemIDs
}
class EmptyFieldDataItemBag(fieldAddr : ItemFieldSpec) extends ShallowFieldDataItemBag(fieldAddr, Set())
// Unordered collection of Items, as well as some of their fields, possibly recursive.
trait DeeperBagData extends FieldDataItemBag {
	// Input is an itemID prev returned by getContainedItemIDs
	def getSomeSubFields(containedItemID : Ident) : Traversable[ItemFieldData]  // Some Subset
}
// Unordered collection of Items, as well as all of their fields, recursively.  Includes all reachable data
trait DeepestBagData extends DeeperBagData {
	// Input is an itemID prev returned by getContainedItemIDs
	def getAllSubFields(containedItemID : Ident) : Traversable[ItemFieldData]  // Exaustive set
}
trait FieldDataFilterFuncs {
	def justFieldDataLeafs(mixedFieldData : Traversable[ItemFieldData]) : Traversable[FieldDataLeaf] =
		mixedFieldData.filter(_.isInstanceOf[FieldDataLeaf]).map(_.asInstanceOf[FieldDataLeaf])

	def justFieldDataBags(mixedFieldData : Traversable[ItemFieldData]) : Traversable[FieldDataItemBag] =
		mixedFieldData.filter(_.isInstanceOf[FieldDataItemBag]).map(_.asInstanceOf[FieldDataItemBag])
}
// RemoteItem = A piece of data that we may read+write as a client of some remote service (e.g. some actor).
// Lower level data-port concept, where we *do* param by CType.
trait RemoteItemField[CType] {
	def getItemFieldSpec : ItemFieldSpec
}
trait SendableRemoteItemField[CType] extends RemoteItemField[CType] {
	def sendContent(c : CType) : Unit
}
trait ReceivableRemoteItemField[CType] extends RemoteItemField[CType]  {
	// def notifyContentUpdate()
	var myChangeHandlers : List[Function1[CType,Unit]] = Nil
	def regChangeHandler(func : Function1[CType,Unit]) : Unit = {
		myChangeHandlers = func :: myChangeHandlers
	}
	def notifyDataChange(upDat : CType) : Unit = {
		// Invokes handlers in this thread
		myChangeHandlers.map(_.apply(upDat))
	}
}

trait ItemDataMsg extends CPumpMsg
trait ItemFieldDataMsg extends ItemDataMsg {
	def getFieldSpec: ItemFieldSpec
}
trait ItemFieldDataChgMsg[CType] extends ItemFieldDataMsg {
	def getUpdatedValue : CType
}

case class ItemFieldDataChgMsgImpl[CType](myItemFieldSpec : ItemFieldSpec, upData : CType)
			extends ItemFieldDataChgMsg[CType] {

	override def getUpdatedValue: CType = upData
	override def getFieldSpec: ItemFieldSpec = myItemFieldSpec
}
// Interesting choice here of whether to parametrize this type.  Currently punting with a Wildcard.
trait RegisterItemFieldInterestMsg extends ItemFieldDataMsg {
	def getNoticeTeler : CPStrongTeller[ItemFieldDataChgMsg[_]]  // Note wildcard.
}
case class RegisterForDataChgNotices(myItemFieldSpec : ItemFieldSpec,
									 myNoticeTeller : CPStrongTeller[ItemFieldDataChgMsg[_]])
			extends  RegisterItemFieldInterestMsg {
	override def getFieldSpec: ItemFieldSpec = myItemFieldSpec

	override def getNoticeTeler: CPStrongTeller[ItemFieldDataChgMsg[_]] = myNoticeTeller
}
// For these data-items, we can presume the serializable message is same in both directions.

case class RemoteItemFieldSenderImpl[CType](myItemFieldSpec : ItemFieldSpec,
									   myRemoteTeller : CPStrongTeller[ItemFieldDataChgMsg[CType]])
			extends SendableRemoteItemField[CType] {
	override def sendContent(c: CType): Unit = {
		val msg = new ItemFieldDataChgMsgImpl[CType](myItemFieldSpec, c)
		myRemoteTeller.tellStrongCPMsg(msg)
	}
	override def getItemFieldSpec: ItemFieldSpec = myItemFieldSpec
}
case class RemoteItemFieldReceiverImpl[CType](myItemFieldSpec : ItemFieldSpec,
									myRemoteTeller : CPStrongTeller[RegisterItemFieldInterestMsg],
									myLocalNoticeTeller : CPStrongTeller[ItemFieldDataChgMsg[_]]		 )
		extends ReceivableRemoteItemField[CType] {
	override def getItemFieldSpec: ItemFieldSpec = myItemFieldSpec

	def registerForNotices(): Unit = {
		val registerMeMsg = new RegisterForDataChgNotices(myItemFieldSpec, myLocalNoticeTeller)
		myRemoteTeller.tellStrongCPMsg (registerMeMsg)
		// But how does myLocalNoticeTeller know about us?
	}
}
/*
class DuplexRemoteItemFieldImpl[CType](myItemFieldSpec : ItemFieldSpec,
								  myRemoteTeller : CPStrongTeller[ItemFieldDataMsg])
			extends SendableRemoteItemField[CType] with ReceivableRemoteItemField[CType] {

	override def getItemFieldSpec: ItemFieldSpec = myItemFieldSpec

	lazy val mySender = new RemoteItemFieldSenderImpl[CType](myItemFieldSpec, myRemoteTeller)
	lazy val myRcvr = new RemoteItemFieldReceiverImpl(myItemFieldSpec, myRemoteTeller)

	override def sendContent(c: CType): Unit = mySender.sendContent(c)

	def registerForChangeNotices(remoteTeller : CPMsgTeller, callbackTeller : CPMsgTeller) : Unit = {
		val registerMeMsg = new RegisterForDataChgNotices(getItemID, getFieldID, callbackTeller)
		myRemoteTeller.tellStrongCPMsg(registerMeMsg)
	}
}
*/

// A partial (or complete, for some purpose) rectangular 2D screen recipe,
// which has a name and may be hidden/shown as a unit ("complete") or sub-unit.
// Subclass to customize the behavior of a group of displayed fields.
trait FlatWidget {  // Is not itself a Gadget, but may produce one
	def getFieldGadgets : List[FlatGadget]
	def getOuterGadgetForMe : FlatGadget
}
trait LayoutRecipe {

}
trait WidgetViewSpec {
	def layoutRectangles(someGadgs : List[FlatGadget]) : List[Rectangle]
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
trait TextLine {
	def getTextLine : String
}
trait UpdatableTextLine extends TextLine {
	def updateTextLine(upTxt : String)
}
case class FixedFlatGadgetTxtLine(id: Ident, fixedTxt : String)
			extends FlatGadgetImpl(id, FunFlatGadgetKinds.FGK_textLine) with TextLine {
	override def getTextLine: String = fixedTxt
}

case class UpdatableTextLineGadget(id : Ident) extends FlatGadgetImpl(id, FunFlatGadgetKinds.FGK_textLine) with UpdatableTextLine {
	var myCachedTxtLine = ""
	override def updateTextLine(upTxt: String): Unit = {myCachedTxtLine = upTxt}

	override def getTextLine: String = myCachedTxtLine
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
	def recordUserChange(udat : FDT) : Unit = ??? // Pull data from UI, send to Gadget.
	// System confirms change by notifying us with equal data.

	// Good chunk to modularize, because it can always be done off rend thrd.
	def makeSpatialTree(layoutAssumps : AnyRef) : JmeNode = ???
}

class FlatFieldDisplayImpl[FDT] extends FlatFieldDisplay[FDT] {
	def getBoundGadget : FlatGadget = ???
}
class FlatWidgetImpl extends UnfinishedFlatWidget {
	val fieldsByGadgetID = new scala.collection.mutable.HashMap[Ident, FlatFieldDisplay[_]]()

	override def getFieldGadgets : List[FlatGadget] = {
		fieldsByGadgetID.values.map(_.asInstanceOf[FlatFieldDisplayImpl[_]].getBoundGadget).toList
	}
	override def insertFieldGadget(fg : FlatGadget): Unit = {
		val fieldDisp = new FlatFieldDisplayImpl
		fieldsByGadgetID.put(fg.getGadgetID, fieldDisp)
	}

	override def getOuterGadgetForMe : FlatGadget = {
		val outer = ???
		outer
	}
}

class FWBodyStatus extends FlatWidgetImpl
trait VWCamStatus
trait VWSpeechStatus
trait VWSensorStatus
