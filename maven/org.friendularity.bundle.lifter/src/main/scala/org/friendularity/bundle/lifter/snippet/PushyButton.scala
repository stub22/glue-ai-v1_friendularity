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
  
	  // Button with no image - the NodeSeq here and in the overloaded method below may go to external resource files eventually
	  def makeButton(buttonText:String, buttonClass:String, buttonId: Int): NodeSeq = {
		val buttonNum: String = buttonId.toString
		<lift:PushyButton buttonId={buttonNum}><div id="pushbutton" class={buttonClass} onclick=""><br/>{buttonText}</div></lift:PushyButton>
		//<lift:PushyButton buttonId={buttonNum}><div id="pushbutton" class={buttonClass} onclick=""><div style="display: table;"><div style="display: table-cell; vertical-align: middle;"><div>{buttonText}</div></div></div></div></lift:PushyButton> // Trying to center vertically: Method 1 in http://blog.themeforest.net/tutorials/vertical-centering-with-css/ - doesn't work!
	  }

	  //Button with image
	  def makeButton(buttonText:String, buttonClass:String, buttonImage:String, buttonId: Int): NodeSeq = {
		val buttonNum: String = buttonId.toString
		val buttonPath: String = "/images/" + buttonImage // May want to move this prefix to central location
		<lift:PushyButton buttonId={buttonNum}><div id="pushbutton" class={buttonClass} onclick=""><img src={buttonPath} width="50%"/><br/>{buttonText}</div></lift:PushyButton>
	  }
  
	  def render = {
		val buttonId: Int = (S.attr("buttonId") openOr "-1").toInt
		"#pushbutton [onclick]" #> SHtml.ajaxInvoke (() => {
			info("Button " + buttonId + " was pressed at " + now)

			buttonId match {
			  // These 99 and 101 IDs are mainly for testing and will likely disappear soon
			  case 99 => {
				  PageCommander.initFromCogcharRDF
				}
			  case 101 => {
				  JsCmds.RedirectTo("/")
				}
				// A special ID which results in an request for Android speech.
				// Yes, a nasty hack. I'd like to get rid of "special case" handling of these pushy button actions soon
			  case 201 => { 
				  PageCommander.requestSpeech
				}
				/* This is for testing of controls reconfiguration once it's working in Cog Char
				 case 202 => {
				 PageCommander.reconfigureControlsFromRdf("anotherLiftConfig.ttl")
				 JsCmds.Noop
				 }
				 */
			  case _ => {
				  info("Starting action mapped to button " + buttonId)
				  val success = PageCommander.triggerCogcharAction(buttonId)
				  if (success) {JsCmds.RedirectTo("cogchar/scene_running.html")}
				}
			}
		  })
	  } 
	  
	}

  }
}

