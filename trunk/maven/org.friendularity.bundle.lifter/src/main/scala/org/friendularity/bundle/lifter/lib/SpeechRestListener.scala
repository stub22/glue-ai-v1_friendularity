package org.friendularity.bundle.lifter {
  package lib {
  
	import model._
	import net.liftweb._
	import common._
	import http._
	import rest._
	import util._
	import Helpers._
	import json._

	object SpeechRestListener extends RestHelper {  
	  serve{ 
		case "speech" :: Nil JsonPut SpeechChunk(chunk) -> _ => SpeechChunk.setContents(chunk): JValue
	  }
      
	}

  }
}
  