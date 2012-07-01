package org.friendularity.bundle.lifter {
  package snippet {

	import scala.xml._	
	import net.liftweb.common._
	import net.liftweb.http._
	import net.liftweb.http.SHtml._
	import net.liftweb.util._
	import net.liftweb.http.js.JsCmd
	import net.liftweb.http.js.JsCmds
	import Helpers._
	import S._
	import org.friendularity.bundle.lifter.model.PageCommander

	object PushyButton extends Logger {
  
	  def makeButton(buttonText:String, buttonClass:String, buttonImage:String, buttonId: Int): NodeSeq = {
		val buttonNum: String = buttonId.toString
		val buttonPath: String = "/images/" + buttonImage // May want to move this prefix to central location
		if (buttonImage.length >= 5) { // needs to be at least this long to have a valid image filename
		  <lift:PushyButton buttonId={buttonNum}><div id="pushbutton" class={buttonClass} onclick=""><img src={buttonPath} width="50%"/><br/>{buttonText}</div></lift:PushyButton>
		} else {
		  <lift:PushyButton buttonId={buttonNum}><div id="pushbutton" class={buttonClass} onclick=""><br/>{buttonText}</div></lift:PushyButton>
		}
	  }
  
	  def render = {
		val buttonId: Int = (S.attr("buttonId") openOr "-1").toInt
		"#pushbutton [onclick]" #> SHtml.ajaxInvoke (() => {
			info("Button " + buttonId + " was pressed at " + now)

			buttonId match {
			  // A special ID used by the SceneInfo screen
			  // These "special cases" probably will eventually be worked out of here
			  // In fact 101 is the last one standing, going away soon
			  case 101 => {
				  JsCmds.RedirectTo("/")
				}
			  case _ => {
				  info("Starting action mapped to button " + buttonId)
				  val processThread = new Thread(new Runnable { // A new thread to call back into PageCommander to make sure we don't block Ajax handling
					  def run() {
						PageCommander.triggerAction(buttonId)
					  }
					})
				  processThread.start
				  JsCmds.Noop
				}
			}
		  })
	  } 
	}
  }
}

