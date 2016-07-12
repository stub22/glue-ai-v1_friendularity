package org.friendularity.navui

import org.appdapter.core.name.{FreeIdent, Ident}
import org.friendularity.dull.RemoteItemField
import org.friendularity.vwimpl.{TextLine, UpdatableTextLineGadget, UpdatableTextLine, IdentHlp, FixedFlatGadgetTxtLine, FlatGadget, FinishedFlatWidget, FlatWidget}

/**
  * Created by Owner on 7/7/2016.
  */

// NavPages are 2D depictions, usually using some transparency, usually attached somewhere under guiNode.
// They may be navigated by user using the NavLogic keybindings.
trait  NavPage {
	def getTopFlatWidget : FlatWidget

}
trait  GadgetMaker extends IdentHlp {
	def makeFixedTxtLine(txt : String) : TextLine = new FixedFlatGadgetTxtLine(makeStampyRandyIdent(), txt)
	def makeUpdatableTxtLine(dataSrcID : Ident) : UpdatableTextLine = {
//		val remote = ???
		val gadg = new UpdatableTextLineGadget(makeStampyRandyIdent())
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
	val myWidg = new FinishedFlatWidget(myFieldGadgs, myOuterGadg)
	override def getTopFlatWidget: FlatWidget = myWidg
}
trait NavPage_Anims {
	// Shows available anims of each kind, highlighting any that are running, and allowing
	// for triggering and cancelling.
}
trait NavPage_Cams {
	// Table showing properties of each cam, mainly pos+dir.  Ops to manipulate cams are lower priority.
}
trait NavPage_Goodies {
	// Shows available goodies of each kind.  Highlight row to select the actual goody in (2 or) 3-space.
	// Also shows stats for goody messages rcvd, total and per-kind and per-goody.
	// Blinks colors on new msg.
}
trait NavPage_Speech {
	// Shows any running speech job, plus some stats.
	// Should also show bookmark info and viseme summary.
	// Eventually can lead into speech config menus.
}
// Additional pages to come:
trait NavPage_Modulators 	// = Content players and behavioral state machines
trait NavPage_Sys			// = Status of JVM, network connections
trait NavPage_Sensors		// = Estim features = the salient point of our structure

