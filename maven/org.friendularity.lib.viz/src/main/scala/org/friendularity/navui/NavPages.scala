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
import org.friendularity.field.{BoundToFieldOfVariableItem, BoundToDataSrc}
import org.friendularity.vwimpl.{BoundPage, OvlPageBook, GadgetMaker, OverlayPage, SvcGate, AttachHlp, FullEnqHlp, TextSpatMaker, MeshGeoMaker, BrushJar, MatPallete, TextLine, UpdatingTextLineGadget, UpdatingTextLine, IdentHlp, FixedFlatGadgetTxtLine, FlatGadget, FinishedFlatWidget, FlatWidget}
import com.jme3.scene.{Node => JmeNode, Mesh}

import scala.collection.mutable.ListBuffer

/**
  * Created by Stub22 on 7/7/2016.
  */

// Hierarchy
// Book
//   Page
//     Entity = one camera, one body, ...
//       Part = one descriptive chunk, such as a vector or color
//         Fragment = one field

trait PageEntrySpec {
	// Defined as a pair of options of gadget-pairs.
	// Contains up to one "Label" and up to one "Dynamic", where the latter could be any kind of single/multi
	// field display+action.
	def getFixedLabelPair_opt : Option[(Ident,String)]
	def getDynamicPair_opt : Option[(Ident,BoundToDataSrc)]
}
trait PageEntryTemplate {

}
trait PartTemplate {

}
trait FragmentDisplayTemplate extends BoundToFieldOfVariableItem {

}
trait EntityDisplayTemplate {

}
class SimplePageEntrySpec(myLabelTxt_opt : Option[String], myBinding_opt : Option[BoundToDataSrc])
					extends PageEntrySpec with IdentHlp {

	lazy val myLabelGadgID = makeStampyRandyIdent()
	lazy val myDynaGadgID = makeStampyRandyIdent()

	//override def getFixedLabelGadgetID_opt = Option(myLabelGadgID)
	//override def getFixedLabelTxt : String = myLabelTxt

	//override def getDynamicGadgetID_opt: Option[Ident] = myBinding_opt.map(x => myDynaGadgID)
	//override def getDynamicGadgetBinding_opt  : Option[BoundToDataSrc] = myBinding_opt
	override def getFixedLabelPair_opt: Option[(Ident, String)] = myLabelTxt_opt.map((myLabelGadgID, _))

	override def getDynamicPair_opt: Option[(Ident, BoundToDataSrc)] = myBinding_opt.map((myDynaGadgID,_))
}
trait PageEntrySpecMaker {
	def makeEntry(labelTxt_opt : Option[String], binding_opt : Option[BoundToDataSrc]) : PageEntrySpec = {
		new SimplePageEntrySpec(labelTxt_opt, binding_opt)
	}
}
trait PageEntryToGadget extends GadgetMaker {
	def toGadgets(pes : PageEntrySpec) : List[FlatGadget] = {
		Nil
	}
}
class SimpleNavPage(outerLabel : String, entries : List[PageEntrySpec]) extends OverlayPage
			with BoundPage with  GadgetMaker {

	val myOuterGadgID = makeStampyRandyIdent()
	val myOuterGadg : FlatGadget = makeFixedTxtLine(myOuterGadgID, outerLabel).asInstanceOf[FlatGadget]

	override def lookupBinding(g : FlatGadget) : Option[BoundToDataSrc] = {
		val ggid = g.getGadgetID
		val item : Option[PageEntrySpec] = entries.find(_.getDynamicPair_opt.map(_._1).getOrElse("").equals(ggid))
		item.flatMap(_.getDynamicPair_opt.map(_._2))
	}
	override def makeTopFlatWidget: FlatWidget =  {
		val gadgList : List[FlatGadget] = Nil
		val tfw = new FinishedFlatWidget(gadgList, {lookupBinding(_).get}, myOuterGadg )
		tfw
	}
	protected def getPageEntrySpecs : List[PageEntrySpec] = entries
	protected def getInsideGadgets : List[FlatGadget] = Nil
}

// We expect VW-picking to eventually interact sensibly with any of these.

/*
class NavPage_BodiesUnused() extends OverlayPage with  GadgetMaker  {
	val b1DataSrcID = makeStampyRandyIdent()
	val b2DataSrcID = makeStampyRandyIdent()

	val u1GadgID = makeStampyRandyIdent()
	val u2GadgID = makeStampyRandyIdent()

	// Table showing gross properties of all bodies, plus we see detail on selected body col/row.
	val myFieldGadgs : List[FlatGadget] = List(
				makeFixedTxtLine(u1GadgID, "body-one"), makeUpdatableTxtLine(b1DataSrcID, 10),
				makeFixedTxtLine(u2GadgID, "body-two"), makeUpdatableTxtLine(b2DataSrcID, 11)).map(_.asInstanceOf[FlatGadget])
	val tgid = makeStampyRandyIdent()
	val myOuterGadg : FlatGadget = makeFixedTxtLine(tgid, "bodies-outer").asInstanceOf[FlatGadget]

	override def makeTopFlatWidget: FlatWidget =  ??? // new FinishedFlatWidget(myFieldGadgs, myOuterGadg)
} */
object NavPageDefs extends PageEntrySpecMaker {

	val page_Bodies = new SimpleNavPage("Bodies", Nil)
	// Table showing gross properties of all bodies, plus we see detail on selected body col/row.

	val camEntries : List[PageEntrySpec] = List (makeEntry(Some("dirVec"), None), makeEntry(Some("pos-X"), None))
	val page_Cams  = new SimpleNavPage("Cams", camEntries) {
		// Table showing properties of each cam, mainly pos+dir.  Ops to manipulate cams are lower priority.
		override protected def getInsideGadgets : List[FlatGadget] = {
			val pgEntrySpcs = getPageEntrySpecs
			val gdgs = pgEntrySpcs.flatMap(pes => {
				Nil
			})
			Nil
		}
	}


	val page_Goodies = new SimpleNavPage("Goodies", Nil)
	// Shows available goodies of each kind.  Highlight row to select the actual goody in (2 or) 3-space.
	// Also shows stats for goody messages rcvd, total and per-kind and per-goody.
	// Blinks colors on new msg.

	val page_Anims = new SimpleNavPage("Anims", Nil)
	// Shows available anims of each kind, highlighting any that are running, and allowing
	// for triggering and cancelling.

	val page_Speech = new SimpleNavPage("Speech", Nil)
	// Shows any running speech job, plus some stats.
	// Should also show bookmark info and viseme summary.
	// Eventually can lead into speech config menus.

	val page_Keys = new SimpleNavPage("Keys", Nil)
	// Show current keybindings, thus equiv to "help"

	val pageList : List[OverlayPage] = List(page_Bodies, page_Cams, page_Goodies, page_Anims, page_Speech, page_Keys)
}
// Additional pages to come:
trait NavPage_Modulators extends OverlayPage  	// = Content players and behavioral state machines
trait NavPage_Sys extends OverlayPage 			// = Status of JVM, network connections
trait NavPage_Sensors extends OverlayPage 		// = Estim features = the salient point of our structure
