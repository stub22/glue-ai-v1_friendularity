package org.friendularity.bundle.lifter {
  package commander {

	import net.liftweb.common._
	import net.liftweb.http.js.JE._
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
	import org.cogchar.render.app.humanoid.SceneActions
	
	object PageCommander extends LiftActor with ListenerManager  {
  
	  private var controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  private var actionsMap = new scala.collection.mutable.HashMap[Int, String]
	  private var updateInfo: Int = 0
	  
	  def createUpdate = updateInfo
	  
	  def getNode(controlId: Int): NodeSeq = {
		if ((controlId == 0) || (controlId == 99)) PushyButton.makeButton("Push to Initalize", "buttongray", 99) else controlsMap(controlId) // We'll get rid of this special init mode soon
	  }
	  
	  def initFromCogcharRDF {
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
			if (controlType.equals(ControlConfig.ControlType.PUSHYBUTTON)) {
			  controlsMap(id) = PushyButton.makeButton(text, style, resource, slotNum)
			  actionsMap(id) = action
			  updateInfo = slotNum
			  updateListeners()
			}
		  })
	  }
	  
	  // Really want to do this via LiftAmbassador, but it can't see into cogchar.lib.render
	  def triggerCogcharScene(id: Int) {
		val triggerBinding = SceneActions.getTriggerBinding(actionsMap(id))
		if (triggerBinding != null) {triggerBinding.perform}  
	  }
	  
	}

  }
}

