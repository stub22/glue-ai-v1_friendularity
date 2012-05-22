package org.friendularity.bundle.lifter {
  package snippet {

	// This snippet handles the "Scene Playing" webapp page
	object SceneInfo {
	  
	  var infoClass = ""
	  var infoImage = ""
	  var infoText = ""

	  
	  def render = {
		val infoImagePath: String = "/images/" + infoImage // May want to move this prefix to central location
		val infoPlayingText: String = "Playing " + infoText
		<div class={infoClass}><img src={infoImagePath} width="50%"/><br/>{infoPlayingText}</div>
	  }
	}

  }
}