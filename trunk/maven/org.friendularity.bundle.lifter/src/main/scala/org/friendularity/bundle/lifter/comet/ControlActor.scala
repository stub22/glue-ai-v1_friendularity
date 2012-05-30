package org.friendularity.bundle.lifter {
  package comet {

	import net.liftweb.common._
	import net.liftweb.http.js.JE._
	import net.liftweb.http._
	import S._
	import net.liftweb.http.js.JsCmd
	import net.liftweb.http.js.JsCmds._
	import net.liftweb.util._
	import Helpers._
	import scala.xml._
	import org.friendularity.bundle.lifter.model.PageCommander

	
	class ControlActor extends CometActor with CometListener {
  
	  lazy val myElementNumber : Int = (name openOr"-1").toInt
	  
	  def registerWith = org.friendularity.bundle.lifter.model.PageCommander
	  
	  override def lowPriority : PartialFunction[Any, Unit]  = {
		case a: Int if (a == myElementNumber) => reRender()
		case _: Int => // Do nothing if our ID not matched  
	  }

	  def render = "@ControlSlot" #> PageCommander.getNode(myElementNumber)
  
	}

  }
}
