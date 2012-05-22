package org.friendularity.bundle.lifter {
  package commander {

	import net.liftweb.common._
	import net.liftweb.http.js.JE._
	import net.liftweb.http.js.JsCmds
	import net.liftweb.http.js.JsCmds._
	import net.liftweb.http._
	import S._
	import net.liftweb.http.SHtml._
	import net.liftweb.util._
	import Helpers._
	import scala.xml._
	import _root_.net.liftweb.util.Log
	import net.liftweb.actor._
	import org.friendularity.bundle.lifter.snippet._
	import org.cogchar.bind.lift._
	import scala.collection.JavaConverters._
	import org.cogchar.platform.trigger.DummyBinding
	
	object PageCommander extends LiftActor with ListenerManager  {
	  private var controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
	  private var controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  
	  private var updateInfo: Int = 0
	  
	  def createUpdate = updateInfo
	  
	  def getNode(controlId: Int): NodeSeq = {
		controlsMap(controlId)
	  }
	  
	  def initFromCogcharRDF { // This could potentially get called more than once but shouldn't usually - may want to add check for that, but shouldn't really hurt anything if it happens
		val controlList: java.util.ArrayList[ControlConfig] = LiftAmbassador.getControls()
		val controlSet = controlList.asScala.toSet
		controlSet.foreach(controlDef => {
			var slotNum:Int = -1
			try {
			  slotNum = controlDef.myURI_Fragment.stripPrefix(LiftAmbassador.getPrefix()).toInt
			} catch {
			  case _: Any =>  // Implies malformed RDF for this control ID: just leave slotNum at -1
			}
			val controlType: ControlConfig.ControlType = controlDef.controlType
			val id = controlDef.id
			val action = controlDef.action
			val text = controlDef.text
			val style = controlDef.style
			val resource = controlDef.resource
			controlDefMap(id) = controlDef; //May or may not turn out to be the best approach long run - saving the control def for actions binding and transfer of info to "Scene Playing" page
			if (controlType.equals(ControlConfig.ControlType.PUSHYBUTTON)) {
			  controlsMap(slotNum) = PushyButton.makeButton(text, style, resource, id)
			  updateInfo = slotNum
			  updateListeners()
			}
		  })
	  }
	  
	  def triggerCogcharScene(id: Int) = {
		val success = LiftAmbassador.triggerScene(controlDefMap(id).action)
		if (success) {setSceneRunningInfo(id)}
		success // not sure the if statement doesn't take care of this, but this does for sure!
	  }
	  
	  def setSceneRunningInfo(id: Int) {
		SceneInfo.infoClass = controlDefMap(id).style
		SceneInfo.infoImage = controlDefMap(id).resource
		SceneInfo.infoText = controlDefMap(id).text
	  }
	  
	  var theMessenger: CogcharMessenger = null // Or None?
	
	  def getMessenger: LiftAmbassador.LiftInterface = { 
		if (theMessenger == null) {
		  theMessenger = new CogcharMessenger
		}
		theMessenger
	  }

	  class CogcharMessenger extends LiftAmbassador.LiftInterface {
		def notifyConfigReady {
		  initFromCogcharRDF
		}
	  }
	
	}

  }
}

