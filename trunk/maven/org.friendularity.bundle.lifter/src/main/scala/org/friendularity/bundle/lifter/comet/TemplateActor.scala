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

	
	class TemplateActor extends CometActor with CometListener {
	  
	  def registerWith = org.friendularity.bundle.lifter.model.PageCommander
	  
	  override def lowPriority : PartialFunction[Any, Unit]  = {
		case 301 => {reRender();} // A special code to trigger a refresh of template
		case _: Int => // Do nothing if our ID not matched
	  }

	  def render = {
		val desiredTemplate = PageCommander.getCurrentTemplate
		"@TemplateSlot" #> <lift:surround with={desiredTemplate} at="content"/>
	  }
  
	}

  }
}
