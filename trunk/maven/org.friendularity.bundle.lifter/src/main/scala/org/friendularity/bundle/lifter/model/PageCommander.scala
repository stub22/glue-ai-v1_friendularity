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
	  // configNames doesn't seem to work, but loading directly from LiftConfigNames does. Not sure if that's because of static fields in an instance or some Java-Scala interop subtlety
	  //private lazy val configNames = LiftAmbassador.getConfigNames() // This loads in an instance of LiftConfigNames from Cogchar so we can look up various RDF action prefixes, etc.
	  private final val COGBOT_TOKEN = "cogbot" // These token definitiions will probably not live here permanently
	  private final val ANDROID_SPEECH_TOKEN = "androidSpeech"
	  
	  private var controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
	  private var controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  
	  // These guys hold lists of slotNums which will display text from Cogbot, or from Android speech input
	  private val cogbotDisplayers = new scala.collection.mutable.ArrayBuffer[Int]
	  private val speechDisplayers = new scala.collection.mutable.ArrayBuffer[Int]
	  
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
	  
	  
	  // IDs, slotNums - someday soon they will be one and the same! This will eliminate all sorts of possible gotchas.
	  // For now we keep the distinction a little longer, because it enables a few workarounds.
	  // A control will always render in the control slot corresponding to its slotNum. Its ID can be anything, but should be unique
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
				  // Check for "local" actions which PageCommander needs to handle, such as text display
				  checkLocalActions(slotNum, action)
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
			  case _ => setControl(slotNum, NodeSeq.Empty); // Blank the control if none of the above
			}
		  })
	  }
					  
	  def setControl(slotNum: Int, slotHtml: NodeSeq) {
		//info("Updating listeners in setControl: Control Slot #" + slotNum)
		controlsMap(slotNum) = slotHtml 
		updateInfo = slotNum
		updateListeners()
	  }
	  
	  // Check to see if any action requested requires PageCommander to do some local handling
	  def checkLocalActions(slotNum:Int, action:String) {
		val splitAction = action.split("_")
		splitAction(0) match {
		  case LiftConfigNames.partial_P_showText => {splitAction(1) match {
				case COGBOT_TOKEN => cogbotDisplayers += slotNum // Show Cogbot speech on this control? Add it to the cogbotDisplayers list.
				case ANDROID_SPEECH_TOKEN => speechDisplayers += slotNum // Add to the speechDisplayers list if we want Android speech shown here
				case _ => warn("checkLocalActions doesn't know what to do in order to display text with token " + splitAction(1))
			  }
			}									
		  case _ => // looks like this action doesn't require anything to happen locally, so do nothing
		}
	  }

	  
	  // A central place to define actions performed by displayed controls - may want to move to its own class eventually
	  // Likely Lift actions will eventually be set up from RDF and/or cogchar actions; having this in PageCommander
	  // will make it natural to reconfigure from Cogchar - next up is a further refinement/generalization of control actions
	  // Just a skeleton so far to demo (ugly hardcoded) Comet control switching
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
	  
	  // Similarly, a central place to handle text input.
	  def textInputMapper(formId:Int, text:String) {
		val desiredAction = controlDefMap(formId).action
		if ((formId > 200) && (formId < 211)) { // This special ID range tells us we are receiving speech from the SpeechRestListener
		  // I have a dream, of a day soon when this special workaround ID for speech will finally be banished, and RDF greatness will reign supreme! But not quite yet.
		  speechDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("I think you said \"" + text + "\"", "", slotNum)))
		} // ... then continue to see if RDF tells us we need to do anything else with speech
		if (desiredAction.startsWith(LiftConfigNames.partial_P_submitText)) { //... otherwise, we don't have a properly defined action for this text input
		  val stringToStrip = LiftConfigNames.partial_P_submitText + "_"
		  val actionToken = desiredAction.stripPrefix(stringToStrip)
		  actionToken match {
			case COGBOT_TOKEN => {
				if (cogbotDisplayers != Nil) { // Likely this check is not necessary - foreach just won't execute if list is Nil, right?
				  val response = LiftAmbassador.getCogbotResponse(text)
				  val cleanedResponse = response.replaceAll("<.*>", ""); // For now, things are more readable if we just discard embedded XML
				  cogbotDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("Cogbot said \"" + cleanedResponse + "\"", "", slotNum)))
				}
			  }
			case _ => warn("No action found in textInputMapper for token " + actionToken)
		  }
		} else {
		  warn("Action in control id " + formId + " is not recognized by textInputMapper")
		}
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
		// A speech request button can currently be declared in RDF by giving it an ID of 201 to 210. (Due to code in PushyButton)
		//info("Updating listeners in requestSpeech")
		updateInfo = 201 // Just to be confusing, this is a different special "ID" - this is actually a slotNum. Death to separate IDs!!
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

