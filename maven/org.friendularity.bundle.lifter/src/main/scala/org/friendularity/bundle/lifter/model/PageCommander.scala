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
	import org.friendularity.bundle.lifter.lib._
	import org.friendularity.bundle.lifter.snippet._
	import org.friendularity.bundle.lifter.view._
	import org.cogchar.bind.lift._
	import scala.collection.JavaConverters._
	import org.cogchar.platform.trigger.DummyBinding
	
	object PageCommander extends LiftActor with ListenerManager with Logger  {
	  private var controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
	  private var controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  
	  private var requestedPage: Option[String] = None // A variable to hold the path to a page requested by LiftAmbassador
	  
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
	  
	  def getRequestedPage = {
		val pageToGet = requestedPage
		requestedPage = None // Once the page is read, the request is complete, so we set this back to Nothing
		pageToGet
	  }
	  
	  
	  def initFromCogcharRDF {
		
		controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
		controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
		
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
			val id = controlDef.id
			val action = controlDef.action
			val text = controlDef.text
			val style = controlDef.style
			val resource = controlDef.resource
			controlDefMap(id) = controlDef; //May or may not turn out to be the best approach long run - saving the control def for actions binding and transfer of info to "Scene Playing" page
			
			controlType match {
			  case ControlType.PUSHYBUTTON => {
				  if (resource.length >= 5) { // needs to be at least this long to have a valid image filename
					setControl(slotNum, PushyButton.makeButton(text, style, resource, id))
				  } else {
					setControl(slotNum, PushyButton.makeButton(text, style, id)) // If no image, use no image constructor
				  }
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
		//info("Updating listeners in setControl: Control Slot #" + slotNum)
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
				case 0 => setControl(6, PushyButton.makeButton("A button (which triggers speech input in Android Proctor)", "buttonred", 201))
				case 1 => setControl(6, TextForm.makeTextForm("A text box", 6))
				case 2 => setControl(6, SelectBoxes.makeSelectBoxes("Checkboxes", List("an option", "and another"), 6))
				case 3 => setControl(6, RadioButtons.makeRadioButtons("Radio buttons", List("Radio Option 1", "Radio Option 2"), 6))
				case _ =>
			  }}
		  case _ => 
		}
	  }	
	  
	  def notifyNewSpeech(speech: String) {
		// What do we want to do with the new speech? It can be anything we want. Probably we'll tell the robot about it!
		// But for now, let's just stick it in a no-image button, which we can use as a text box.
		// Essentially this is just a piece of demo config right now, but this is the place from which we can wire speech into Cogchar.
		setControl(12, PushyButton.makeButton("I think you said \"" + speech + "\"", "", 12))
	  }
	  
	  def triggerCogcharAction(id: Int) = {
		var success = false;
		if (controlDefMap.contains(id)) {
		  success = LiftAmbassador.triggerAction(controlDefMap(id).action)
		  // In case this is a scene and Cog Char tells us to show the info page, be sure it has the required info 
		  setSceneRunningInfo(id) // eventually this may not be necessary, and Cog Char may handle this part too}
		}
		success
	  }
	  
	  def setSceneRunningInfo(id: Int) {
		SceneInfo.infoClass = controlDefMap(id).style
		SceneInfo.infoImage = controlDefMap(id).resource
		SceneInfo.infoText = controlDefMap(id).text
	  }
	  
	  def requestSpeech {
		// May not be the cleanest solution in the long run, but for now special requests like this are handled with special "control" IDs
		// A speech request button can currently be declared in RDF by giving it an ID of 201. (Due to code in PushyButton)
		//info("Updating listeners in requestSpeech")
		updateInfo = 201
		updateListeners()
	  }
	   
	  def reconfigureControlsFromRdf(rdfFile:String) = {
		LiftAmbassador.activateControlsFromRdf(rdfFile)
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
		def loadPage(pagePath:String) {
		  //info("Updating listeners in loadPage")
		  requestedPage = Some(pagePath)
		  updateInfo = 202 // Our "special control slot" for triggering page redirect
		  updateListeners()
		}
	  }
	
	}

  }
}

