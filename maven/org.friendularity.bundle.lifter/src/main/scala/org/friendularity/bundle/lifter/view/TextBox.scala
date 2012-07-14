package org.friendularity.bundle.lifter {
  package view {

	import scala.xml._	
	import net.liftweb.http._
	import net.liftweb.util._
	import Helpers._

	object TextBox {
  
	  def makeBox(text:String, style:String, centered:Boolean): NodeSeq = {
		if (centered) {
		  <div class="centerVert"><div class={style}>{text}</div></div>
		} else {
		  <div class={style}>{text}</div>
		}
	  }
	  
	  def makeBox(text:String, style:String): NodeSeq = {
		makeBox(text, style, false);
	  }

	}
  }
}

