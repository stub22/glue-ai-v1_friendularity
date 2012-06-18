package org.friendularity.bundle.lifter {
  package comet {

	import net.liftweb.common._
	import net.liftweb.http.js.JE._
	import net.liftweb.http._
	import net.liftweb.http.js.JsCmd
	import net.liftweb.http.js.JsCmds._
	import net.liftweb.util._
	import Helpers._
	import scala.xml._
	import java.util.Date

	object SpeechRequestActor {  
	  var lastReqTime: Long = 0;
	}

	class SpeechRequestActor extends CometActor with CometListener with Logger {

	  override def defaultPrefix = Full("speechReq")  
      
	  def render = bind("SpeechReqPush" -> "") // On initial render, just blank anything in SpeechRequestActor comet div
  
	  def registerWith = org.friendularity.bundle.lifter.model.PageCommander

	  override def lowPriority : PartialFunction[Any, Unit] = {
		SpeechRequestActor.synchronized {
		  case 201 => { // A special "slot" code for this actor. Sort of a workaround, but works OK for now.
			  partialUpdate(new JsCmd { 
				  def toJsCmd = "Android.getSpeechInput();"
				})
			}
		  case _ => // Do nothing for other IDs
		}
	  }
	}
  }
}