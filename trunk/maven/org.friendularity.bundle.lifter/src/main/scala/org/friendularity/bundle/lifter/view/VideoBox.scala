package org.friendularity.bundle.lifter {
  package view {

	import scala.xml._	
	import net.liftweb.util._
	import Helpers._
	import net.liftweb.http._

	object VideoBox {
  
	  def makeBox(videoResource:String, mute: Boolean): NodeSeq = {
		val videoPath: String = "/video/" + videoResource // May want to move this prefix to central location
		val muteText = mute.toString
		// It's all well and good to use a single video resource unless we want to support IE, in which case we'll have to mix in more
		<video src={videoPath} width="100%" height="100%" autoplay="true" muted={muteText}></video>
	  } 
	}
	
  }
}

