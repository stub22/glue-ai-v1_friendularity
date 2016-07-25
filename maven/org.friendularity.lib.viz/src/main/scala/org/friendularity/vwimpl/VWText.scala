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

import com.jme3.font.BitmapText
import com.jme3.scene.Spatial
import org.appdapter.core.name.Ident

/**
  * Created by Stub22 on 7/11/2016.
  */
// Big trick is that we can use these params to help size things that are not really text, too.
// With that said, their translation even to acutal text pixel sizes is nontrivial.
trait TextBlockSizeParams {
	// All measurements in presumed ideal chars
	def getMinHeight : Int = 1
	def getPreferredHeight : Int = 2
	def getMaxHeight : Int = 10
	def getMinWidth : Int = 4
	def getMaxWidth : Int = 80
	def getPreferredWidth : Int = 24

}
// One line might be one word or many, but always at least removes concerns about interline spacing.
trait OneLineTextBlockSizeParams extends TextBlockSizeParams {
	// With these two settings, all 3 heights = {min, preferred, max} are 1
	override def getMaxHeight : Int = 1
	override def getPreferredHeight : Int = 1
}
case class OneLineTxtBlkSzHints(minWidthChrs : Option[Int], prefWidthChrs : Option[Int], maxWidthChrs : Option[Int])
			extends OneLineTextBlockSizeParams {
	override def getMinWidth : Int = minWidthChrs.getOrElse(super.getMinWidth)
	override def getPreferredWidth : Int = prefWidthChrs.getOrElse(super.getPreferredWidth)
	override def getMaxWidth : Int = maxWidthChrs.getOrElse(super.getMaxWidth)
}
// Instructive discussion about text sizing here:
// https://hub.jmonkeyengine.org/t/bitmaptexts-getheight-and-getlinewidth-not-accurate/29953/6

trait TextLine {
	def getTextLine : String
}
// TextLine is never gui-editable by the user (see instead ____), rather, it is just updating from system data events.
trait UpdatingTextLine extends TextLine {
	def updateTextLine(upTxt : String)
}
class FixedFlatGadgetTxtLine(gadgetID: Ident, fixedTxt : String)
			extends FlatGadgetImpl(gadgetID, FunFlatGadgetKinds.FGK_textLine) with TextLine {
	override def getTextLine: String = fixedTxt

	val textLen = fixedTxt.length
	override def getSizeHints : TextBlockSizeParams = new OneLineTxtBlkSzHints(Some(textLen / 2), Some(textLen), Some(textLen))
	override def getSpat(odh : OvlDisplayHelp) : Spatial = {
		val txtMaker = odh.happyTxtMaker
		val someBT : BitmapText = txtMaker.makeBitmapTxt2D(fixedTxt)
		val scaleFactor = 7.0f
		someBT.scale(scaleFactor, scaleFactor, 1.0f)
		someBT
	}
}

// Usually we want size to be fixed, rather than changing when text updates.
class UpdatingTextLineGadget(id : Ident, sizeHints : TextBlockSizeParams) extends FlatGadgetImpl(id, FunFlatGadgetKinds.FGK_textLine) with UpdatingTextLine {
	var myCachedTxtLine = ""
	override def updateTextLine(upTxt: String): Unit = {myCachedTxtLine = upTxt}

	override def getTextLine: String = myCachedTxtLine

	override def getSpat(odh : OvlDisplayHelp) : Spatial = {
		null
	}

	override def getSizeHints: TextBlockSizeParams = sizeHints
}
