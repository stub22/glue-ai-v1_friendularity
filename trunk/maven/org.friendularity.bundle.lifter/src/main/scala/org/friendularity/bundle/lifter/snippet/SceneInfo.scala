package org.friendularity.bundle.lifter {
  package snippet {
	
	import scala.xml._	
	import net.liftweb.http.js.JsCmds
	import net.liftweb.util._
	import Helpers._
	import net.liftweb.http._
	import net.liftweb.http.SHtml._
	import S._

	// This snippet handles the "Scene Playing" webapp page
	object SceneInfo {
	  
	  var infoClass = ""
	  var infoImage = ""
	  var infoText = ""

	  
	  def render = {
		val infoImagePath: String = "/images/" + infoImage // May want to move this prefix to central location
		val infoPlayingText: String = "Playing " + infoText
		<lift:PushyButton buttonId="101"><div id="pushbutton" class={infoClass} onclick=""><br/><img src={infoImagePath} width="50%"/><br/>{infoPlayingText}</div></lift:PushyButton>
	  }
	  
	}

  }
}