package org.friendularity.bundle.lifter {
  package model {

// A resource of action string components interpreted by PageCommander
	object ActionStrings {
	  final val getContinuousSpeech = "startgetspeech"
	  final val stopContinuousSpeech = "stopgetspeech"
	  final val acquireSpeech = "getspeech"
	  final val cogbotSpeech = "cogbotspeech"
	  final val submitText = "submittext"
	  final val showText = "showtext"
	  final val setVariable = "setvariable"
	  final val oldDemo = "olddemo"
	  
	  final val COGBOT_TOKEN = "cogbot" // These token definitiions will probably not live here permanently
	  final val ANDROID_SPEECH_TOKEN = "androidSpeech" // for Android speech recognition
	  final val ENABLE_TOKEN = "enable" 
	  final val DISABLE_TOKEN = "disable"
	}

  }
}