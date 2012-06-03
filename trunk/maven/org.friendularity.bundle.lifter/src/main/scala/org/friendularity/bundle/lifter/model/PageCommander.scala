package org.friendularity.bundle.lifter {
  package model {

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
	import org.friendularity.bundle.lifter.view._
	import org.cogchar.bind.lift._
	import scala.collection.JavaConverters._
	import org.cogchar.platform.trigger.DummyBinding
	
	object PageCommander extends LiftActor with ListenerManager  {
	  private var controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
	  private var controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  
	  private var updateInfo: Int = 0
	  
	  def createUpdate = updateInfo
	  
	  // A list of possible control types
	  object ControlType extends Enumeration { 
		type ControlType = Value
		val NULLTYPE, PUSHYBUTTON, TEXTINPUT, SELECTBOXES, RADIOBUTTONS, LISTBOX, VIDEOBOX = Value
	  }
	  import ControlType._
	  
	  def getNode(controlId: Int): NodeSeq = {
		var nodeOut = NodeSeq.Empty
		try {
		  nodeOut = controlsMap(controlId)
		} catch {
		  case _: Any => // Implies nothing in map for this controlId, do nothing and return empty nodeOut
		}
		nodeOut
	  }
	  
	  def initFromCogcharRDF { // This could potentially get called more than once but shouldn't usually - may want to add check for that, but shouldn't really hurt anything if it happens
		val controlList: java.util.ArrayList[ControlConfig] = LiftAmbassador.getControls()
		val controlSet = controlList.asScala.toSet
		controlSet.foreach(controlDef => {
			var slotNum:Int = -1
			try {
			  slotNum = controlDef.myURI_Fragment.stripPrefix(LiftAmbassador.getPrefix()).toInt
			} catch {
			  case _: Any =>  // Implies malformed RDF for this control slot: just leave slotNum at -1
			}
			var controlType: ControlType = NULLTYPE
			ControlType.values foreach(testType => {
				if (controlDef.controlType equals(testType.toString)) controlType = testType
			  })
			println("Control type has been found to be " + controlType)
			val id = controlDef.id
			val action = controlDef.action
			val text = controlDef.text
			val style = controlDef.style
			val resource = controlDef.resource
			controlDefMap(id) = controlDef; //May or may not turn out to be the best approach long run - saving the control def for actions binding and transfer of info to "Scene Playing" page
			
			controlType match {
			  case ControlType.PUSHYBUTTON => {
				  setControl(slotNum, PushyButton.makeButton(text, style, resource, id))
				}
			  case ControlType.TEXTINPUT => {
				  setControl(slotNum, TextForm.makeTextForm(text, id))
				}
			  case ControlType.SELECTBOXES => {
				  // From the RDF "text" value we assume a comma separated list with the first item the title and the rest checkbox labels
				  val textItems = List.fromArray(text.split(","))
				  val titleText = textItems(0)
				  val labelItems = textItems.tail
				  setControl(slotNum, SelectBoxes.makeSelectBoxes(titleText, labelItems, id))
				}
			  case ControlType.RADIOBUTTONS => {
				  // From the RDF "text" value we assume a comma separated list with the first item the title and the rest radiobutton labels
				  val textItems = List.fromArray(text.split(","))
				  val titleText = textItems(0)
				  val labelItems = textItems.tail
				  setControl(slotNum, RadioButtons.makeRadioButtons(titleText, labelItems, id))
				}
			  case ControlType.LISTBOX => {
				  // From the RDF "text" value we assume a comma separated list with the first item the title and the rest radiobutton labels
				  val textItems = List.fromArray(text.split(","))
				  val titleText = textItems(0)
				  val labelItems = textItems.tail
				  setControl(slotNum, ListBox.makeListBox(titleText, labelItems, id))
				}
			  case ControlType.VIDEOBOX => {
				  setControl(slotNum, VideoBox.makeBox(resource, true)) // Videos muted for now, but we can change and/or add config from RDF as desired
				}
			  case _ => // No action if no match
			}
		  })
	  }
					  
	  def setControl(slotNum: Int, slotHtml: NodeSeq) {
		controlsMap(slotNum) = slotHtml 
		updateInfo = slotNum
		updateListeners()
	  }
	  
	  // A central place to define actions performed by displayed controls - may want to move to its own class eventually
	  // Likely Lift actions will eventually be set up from RDF and/or cogchar actions; having this in PageCommander
	  // will make it natural to reconfigure from Cogchar - next up is a further refinement/generalization of control actions
	  // Just a skeleton so far to demo Comet control switching
	  def controlActionMapper(formId:Int, subControl:Int) {
		formId match {
		  case 5 => {subControl match {
				case 0 => setControl(6, PushyButton.makeButton("A button", "buttonred", "still-27.jpg", 6))
				case 1 => setControl(6, TextForm.makeTextForm("A text box", 6))
				case 2 => setControl(6, SelectBoxes.makeSelectBoxes("Checkboxes", List("an option", "and another"), 6))
				case 3 => setControl(6, RadioButtons.makeRadioButtons("Radio buttons", List("Radio Option 1", "Radio Option 2"), 6))
				case _ =>
			  }}
		  case _ => 
		}
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
	  
	  var theMessenger: CogcharMessenger = null
	
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
