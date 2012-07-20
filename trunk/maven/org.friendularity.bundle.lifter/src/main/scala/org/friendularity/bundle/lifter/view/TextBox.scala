package org.friendularity.bundle.lifter {
  package view {

	import scala.xml._	
	import net.liftweb.http._
	import net.liftweb.util._
	import Helpers._

	object TextBox {
  
	  def makeBox(text:String, style:String, centered:Boolean, displayAsCell:Boolean): NodeSeq = {
		if (centered) {
		  if (displayAsCell) {
			<div class="centerVert"><div class={style}>{text}</div></div>
		  } else {
			// The extra div makes the contents not display as a table cell, for example, for when we don't want background to fill whole cell
			<div class="centerVert"><div><div class={style}>{text}</div></div></div> 
		  }
		  
		} else {
		  <div class={style}>{text}</div>
		}
	  }
	  
	  def makeBox(text:String, style:String, centered:Boolean): NodeSeq = {
		makeBox(text, style, centered, true)
	  }
	  
	  def makeBox(text:String, style:String): NodeSeq = {
		makeBox(text, style, false);
	  }

	}
  }
}

