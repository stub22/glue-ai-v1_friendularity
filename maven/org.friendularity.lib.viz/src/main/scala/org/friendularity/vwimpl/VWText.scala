package org.friendularity.vwimpl

import org.appdapter.core.name.Ident

/**
  * Created by Stub22 on 7/11/2016.
  */
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
trait TextLine {
	def getTextLine : String
}
// TextLine is never gui-editable by the user (see instead ____), rather, it is just updating from system data events.
trait UpdatingTextLine extends TextLine {
	def updateTextLine(upTxt : String)
}
case class FixedFlatGadgetTxtLine(id: Ident, fixedTxt : String)
			extends FlatGadgetImpl(id, FunFlatGadgetKinds.FGK_textLine) with TextLine {
	override def getTextLine: String = fixedTxt
}

case class UpdatingTextLineGadget(id : Ident) extends FlatGadgetImpl(id, FunFlatGadgetKinds.FGK_textLine) with UpdatingTextLine {
	var myCachedTxtLine = ""
	override def updateTextLine(upTxt: String): Unit = {myCachedTxtLine = upTxt}

	override def getTextLine: String = myCachedTxtLine
}
