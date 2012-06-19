package org.friendularity.bundle.lifter {
  package comet {

	import net.liftweb.common._
	import net.liftweb.http.js.JE._
	import net.liftweb.http._
	import net.liftweb.http.js.JsCmd
	import net.liftweb.http.js.JsCmds
	import net.liftweb.util._
	import Helpers._
	import scala.xml._
	import java.util.Date
	import org.friendularity.bundle.lifter.model.PageCommander

	object JavaScriptActor {  
	  var lastReqTime: Long = 0;
	}

	class JavaScriptActor extends CometActor with CometListener with Logger {

	  override def defaultPrefix = Full("js_command")  
      
	  def render = bind("CommandPush" -> "") // On initial render, just blank anything in JavaScriptActor comet div
  
	  def registerWith = org.friendularity.bundle.lifter.model.PageCommander

	  override def lowPriority : PartialFunction[Any, Unit] = {
		case 201 => { // A special "slot" code for speech request. Sort of a workaround, but works OK for now.
			partialUpdate(new JsCmd { 
				def toJsCmd = "Android.getSpeechInput();"
			  })
		  }
		case 202 => { // A special "slot" code for page redirect.
			val newPage = PageCommander.getRequestedPage
			newPage match {
			  case Some(page) => partialUpdate(JsCmds.RedirectTo(page))
			  case None => JsCmds.Noop
			}  
		  }	
		case _ => // Do nothing for other IDs
	  }
	}
  }
}
