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
	import java.util.concurrent.{Executors, TimeUnit}
	
	object PageCommander extends LiftActor with ListenerManager with Logger  {
	  // configNames doesn't seem to work, but loading directly from LiftConfigNames does. Not sure if that's because of static fields in an instance or some Java-Scala interop subtlety
	  //private lazy val configNames = LiftAmbassador.getConfigNames() // This loads in an instance of LiftConfigNames from Cogchar so we can look up various RDF action prefixes, etc.
	  private final val COGBOT_TOKEN = "cogbot" // These token definitiions will probably not live here permanently
	  private final val ANDROID_SPEECH_TOKEN = "androidSpeech" // for Android speech recognition
	  private final val ENABLE_TOKEN = "enable" 
	  private final val DISABLE_TOKEN = "disable"
	  
	  private val controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
	  private val controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  private val singularAction = new scala.collection.mutable.HashMap[Int, String] // Holds action for currently enabled state of a multi-state control, such as a TOGGLEBUTTON
	  
	  // These guys hold lists of slotNums which will display text from Cogbot, or from Android speech input
	  private val cogbotDisplayers = new scala.collection.mutable.ArrayBuffer[Int]
	  private val speechDisplayers = new scala.collection.mutable.ArrayBuffer[Int]
	  // ... and this holds lists of slotNums which should trigger Android spech input
	  private val speechGetters = new scala.collection.mutable.ArrayBuffer[Int] // Will be factoring this out and handling more like continuous speech
	  private val continuousSpeechGetters = new scala.collection.mutable.ArrayBuffer[Int] // ... but for now will need this too, what a mess! Need to clean up this logic.
	  // ... this one for ToggleButtons
	  private val toggleButtonMap =  new scala.collection.mutable.HashMap[Int, Boolean]
	  
	  private var requestedPage: Option[String] = None // A variable to hold the path to a page requested by LiftAmbassador
	  private var outputSpeech: String = "" // Holds speech we want Android to say
	  private var lastSpeechReqSlotNum = 0 // slotNum of last control which requested speech - used by JavaScriptActor to add identifying info to request
	  private var cogbotSpeaks = false // Determines whether Cogbot speech out also triggers Android speech
	  
	  private var updateInfo: Int = 0
	  
	  def createUpdate = updateInfo
	  
	  // A list of possible control types
	  object ControlType extends Enumeration { 
		type ControlType = Value
		val NULLTYPE, PUSHYBUTTON, TEXTINPUT, SELECTBOXES, RADIOBUTTONS, LISTBOX, VIDEOBOX, TOGGLEBUTTON = Value
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
		
		// We should reset all this stuff
		controlDefMap.clear
		controlsMap.clear
		cogbotDisplayers.clear
		speechDisplayers.clear
		speechGetters.clear
		continuousSpeechGetters.clear
		toggleButtonMap.clear
		singularAction.clear
		
		val controlList: java.util.ArrayList[ControlConfig] = LiftAmbassador.getControls()
		val controlSet = controlList.asScala.toSet
		controlSet.foreach(controlDef => {
			var slotNum:Int = -1
			try {
			  slotNum = controlDef.myURI_Fragment.stripPrefix(LiftAmbassador.getControlPrefix()).toInt
			} catch {
			  case _: Any =>  warn("Unable to get valid slotNum from loaded control; URI fragment was " + controlDef.myURI_Fragment) // The control will still be loaded into slot -1; could "break" here but it's messy and unnecessary
			}
			var controlType: ControlType = NULLTYPE
			ControlType.values foreach(testType => {
				if (controlDef.controlType equals(testType.toString)) controlType = testType
			  })
			val action = controlDef.action
			val text = controlDef.text
			val style = controlDef.style
			val resource = controlDef.resource
			
			
			controlType match {
			  case ControlType.PUSHYBUTTON => {
				  setControl(slotNum, PushyButton.makeButton(text, style, resource, slotNum))
				  // Check for "local" actions which PageCommander needs to handle, such as text display
				  controlDef.action = initLocalActions(slotNum, action) // this method will modify action as necessary according to prefixes
				}
			  case ControlType.TEXTINPUT => {
				  setControl(slotNum, TextForm.makeTextForm(text, slotNum))
				}
			  case ControlType.SELECTBOXES => {
				  // From the RDF "text" value we assume a comma separated list with the first item the title and the rest checkbox labels
				  val textItems = List.fromArray(text.split(","))
				  val titleText = textItems(0)
				  val labelItems = textItems.tail
				  setControl(slotNum, SelectBoxes.makeSelectBoxes(titleText, labelItems, slotNum))
				}
			  case ControlType.RADIOBUTTONS => {
				  // From the RDF "text" value we assume a comma separated list with the first item the title and the rest radiobutton labels
				  val textItems = List.fromArray(text.split(","))
				  val titleText = textItems(0)
				  val labelItems = textItems.tail
				  setControl(slotNum, RadioButtons.makeRadioButtons(titleText, labelItems, slotNum))
				}
			  case ControlType.LISTBOX => {
				  // From the RDF "text" value we assume a comma separated list with the first item the title and the rest radiobutton labels
				  val textItems = List.fromArray(text.split(","))
				  val titleText = textItems(0)
				  val labelItems = textItems.tail
				  setControl(slotNum, ListBox.makeListBox(titleText, labelItems, slotNum))
				}
			  case ControlType.VIDEOBOX => {
				  setControl(slotNum, VideoBox.makeBox(resource, true)) // Videos muted for now, but we can change and/or add config from RDF as desired
				}
			  case ControlType.TOGGLEBUTTON => {
				  // For a ToggleButton, the first item in CSV text, action, style, image corresponds to the default condition, the second to the "toggled" condition
				  val textItems = List.fromArray(text.split(","))
				  val styleItems = List.fromArray(style.split(","))
				  val resourceItems = List.fromArray(resource.split(","))
				  // Set control for initial (default) state
				  setControl(slotNum, PushyButton.makeButton(textItems(0), styleItems(0), resourceItems(0), slotNum))
				  // Flag the fact this is a toggle button, currently in the default (false) condition
				  toggleButtonMap(slotNum) = false
				}
			  case _ => setControl(slotNum, NodeSeq.Empty); // Blank the control if none of the above
			}
			controlDefMap(slotNum) = controlDef; //Save the controlDef for this slotNum for future reference
		  })
	  }
					  
	  def setControl(slotNum: Int, slotHtml: NodeSeq) {
		//info("Updating listeners in setControl: Control Slot #" + slotNum)
		controlsMap(slotNum) = slotHtml 
		updateInfo = slotNum
		updateListeners()
	  }
	  
	  // Check to see if any action requested requires PageCommander to do some local handling
	  def initLocalActions(slotNum:Int, action:String):String = {
		var adjustedAction = action
		val splitAction = action.split("_")
		splitAction(0) match {
		  case LiftConfigNames.partial_P_acquireSpeech => { // Special handling for this one! It wants us to acquire speech
			  speechGetters += slotNum // Add this slotNum to the list of speechGetters!
			  val replacedText = LiftConfigNames.partial_P_acquireSpeech + "_"
			  adjustedAction = action.stripPrefix(replacedText) // Strip the getspeech prefix, now the remaining action can be processed as usual
			}
		  case LiftConfigNames.partial_P_showText => {splitAction(1) match {
				case COGBOT_TOKEN => cogbotDisplayers += slotNum // Show Cogbot speech on this control? Add it to the cogbotDisplayers list.
				case ANDROID_SPEECH_TOKEN => speechDisplayers += slotNum // Add to the speechDisplayers list if we want Android speech shown here
				case _ => warn("checkLocalActions doesn't know what to do in order to display text with token " + splitAction(1))
			  }
			}			  
		  case _ => // looks like this action doesn't require anything to happen locally, so do nothing
		}
		adjustedAction // This is returned, in case action needed to be modified
	  }

	  
	  // A central place to define actions performed by displayed controls - may want to move to its own class eventually
	  // Likely Lift actions will eventually be set up from RDF and/or cogchar actions; having this in PageCommander
	  // will make it natural to reconfigure from Cogchar - next up is a further refinement/generalization of control actions
	  // Just a skeleton so far to demo (ugly hardcoded) Comet control switching
	  def controlActionMapper(formId:Int, subControl:Int) {
		formId match {
		  case 5 => {subControl match {
				case 0 => setControl(6, PushyButton.makeButton("A button", "buttonred", "", 6))
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
		var desiredAction = controlDefMap(formId).action
		if (singularAction contains formId) {desiredAction = singularAction(formId)} // If this is a "multi-state" control, get the action corresponding to current state
		// If we see an action with the getContinuousSpeech command, strip that off and see what's behind
		if (desiredAction startsWith ActionStrings.getContinuousSpeech) {desiredAction = desiredAction.stripPrefix(ActionStrings.getContinuousSpeech + "_")}
		//info("In textInputMapper; desiredAction is " + desiredAction) // TEST ONLY
		/* This may work after refactoring - problem now is that acquireSpeech prefix has been stripped in initLocalActions
		 if (desiredAction.startsWith(LiftConfigNames.partial_P_acquireSpeech) || desiredAction.startsWith(ActionStrings.getContinuousSpeech)) { // If it does, we are receiving speech from the SpeechChunk
		 speechDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("I think you said \"" + text + "\"", controlDefMap(slotNum).style, "", slotNum)))
		 } // ... then continue to see if RDF tells us we need to do anything else with speech
		 */
		if ((speechGetters contains formId) || (continuousSpeechGetters contains formId)) { // If it does, we are receiving speech from the SpeechChunk
		  speechDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("I think you said \"" + text + "\"", controlDefMap(slotNum).style, "", slotNum)))
		} // ... then continue to see if RDF tells us we need to do anything else with speech
		if (desiredAction.startsWith(LiftConfigNames.partial_P_submitText)) { //... otherwise, we don't have a properly defined action for this text input
		  val stringToStrip = LiftConfigNames.partial_P_submitText + "_"
		  val actionToken = desiredAction.stripPrefix(stringToStrip)
		  actionToken match {
			case COGBOT_TOKEN => {
				if (cogbotDisplayers != Nil) { // Likely this check is not necessary - foreach just won't execute if list is Nil, right?
				  val response = LiftAmbassador.getCogbotResponse(text)
				  val cleanedResponse = response.replaceAll("<.*>", ""); // For now, things are more readable if we just discard embedded XML
				  cogbotDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("Cogbot said \"" + cleanedResponse + "\"", controlDefMap(slotNum).style, "", slotNum)))
				  if (cogbotSpeaks) outputSpeech(cleanedResponse) // Output Android speech if cogbotSpeaks is set
				  // It used to seem necessary to invoke an arbitrary delay first for this to work following Android speech recognition.
				  // This seems fixed by using an alternate speech recognition technique in HRK Android Viewer. Leaving this for now for possible future debugging:
				  /*
				   lazy val sched = Executors.newSingleThreadScheduledExecutor();
				   sched.schedule(new Runnable {
				   def run = {
				   outputSpeech(cleanedResponse) 
				   }
				   }, 5000, TimeUnit.MILLISECONDS);
				   */
				}
			  }
			case _ => warn("No action found in textInputMapper for token " + actionToken)
		  }
		} else {
		  warn("Action in control id " + formId + " is not recognized by textInputMapper: " + desiredAction)
		}
	  }
	  
	  // Perform action according to slotNum 
	  def triggerAction(id: Int): Boolean  = {
		var success = false
		if (toggleButtonMap contains id) {success = toggleButton(id)}
		else {success = continueTriggering(id)}
		success
	  }
	  
	  // Another "segment" of the triggering operation, which we jump back into from toggleButton
	  def continueTriggering(id: Int): Boolean = {
		var success = false
		if (controlDefMap.contains(id)) {
		  if (performLocalActions(id)) success = true // Local action was performed! We're done.
		  else {
			var action = controlDefMap(id).action
			if (singularAction contains id) {action = singularAction(id)} // If this is a "multi-state" control, get the action corresponding to current state
			//info("In continueTriggering, acting on action " + action) // TEST ONLY
			success = LiftAmbassador.triggerAction(action)
			// In case this is a scene and Cog Char tells us to show the info page, be sure it has the required info 
			setSceneRunningInfo(id) // eventually this may not be necessary, and Cog Char may handle this part too}
		  }
		} else {warn("Action requested, but no control def found for slot " + id)}
		success
	  }
	  
	  def setSceneRunningInfo(id: Int) {
		SceneInfo.infoClass = controlDefMap(id).style
		SceneInfo.infoImage = controlDefMap(id).resource
		SceneInfo.infoText = controlDefMap(id).text
	  }
	  
	  // Perform any button actions handled locally, and return true if we find one
	  def performLocalActions(slotNum: Int) = {
		var action = controlDefMap(slotNum).action
		if (singularAction contains slotNum) {action = singularAction(slotNum)} // If this is a "multi-state" control, get the action corresponding to current state
		//info("In performLocalActions, acting on action " + action) // TEST ONLY
		var success = false;
		if (speechGetters contains slotNum) { // Check to see if this control handles speech input
		  updateInfo = 201 // Special "slotNum" to tell JavaScriptActor to request speech
		  lastSpeechReqSlotNum = slotNum; // Set this field - JavaScriptActor will use it to attach requesting info to JS Call - allows multiple speech request controls
		  updateListeners()
		  success = true
		} else { // Check for other actions defined in action string
		  val splitAction = action.split("_")
		  splitAction(0) match {
			case LiftConfigNames.partial_P_cogbotSpeech => splitAction(1) match {
				case ENABLE_TOKEN => cogbotSpeaks = true; success = true
				case DISABLE_TOKEN => cogbotSpeaks = false; success = true
				case _ => // No match, just exit (success=false)
			  }
			case ActionStrings.getContinuousSpeech => {
				requestContinuousSpeech(slotNum, true)
				continuousSpeechGetters += slotNum;
			  }
			case ActionStrings.stopContinuousSpeech => {
				requestContinuousSpeech(slotNum, false)
				continuousSpeechGetters -= slotNum;
			  }
			case _ => // No match, just exit (success=false)
		  }
		}
		success
	  }
	  
	  def toggleButton(slotNum: Int) = {
		var success = false;
		if (controlDefMap contains slotNum) {
		  if (controlDefMap(slotNum).controlType equals ControlType.TOGGLEBUTTON.toString) {
			var textItems = List.fromArray(controlDefMap(slotNum).text.split(","))
			var actionItems = List.fromArray(controlDefMap(slotNum).action.split(","))
			var styleItems = List.fromArray(controlDefMap(slotNum).style.split(","))
			var resourceItems = List.fromArray(controlDefMap(slotNum).resource.split(","))
			if (toggleButtonMap contains slotNum) {
			  if (toggleButtonMap(slotNum)) {
				// Button is "selected" -- change back to "default" and perform action
				singularAction(slotNum) = actionItems(1)
				success = continueTriggering(slotNum)
				setControl(slotNum, PushyButton.makeButton(textItems(0), styleItems(0), resourceItems(0), slotNum))
				toggleButtonMap(slotNum) = false
			  } else {
				// Button is set as "default" -- set to "selected" and perform action
				// If only one parameter is specified in RDF, duplicate the first and use that parameter here too (really we are prepending the one item in the list to itself, but that works ok here)
				if (textItems.length < 2) textItems ::= textItems(0)
				if (actionItems.length < 2) actionItems ::= actionItems(0)
				if (styleItems.length < 2) styleItems ::= styleItems(0)
				if (resourceItems.length < 2) resourceItems ::= resourceItems(0)
				singularAction(slotNum) = actionItems(0)
				success = continueTriggering(slotNum)
				setControl(slotNum, PushyButton.makeButton(textItems(1), styleItems(1), resourceItems(1), slotNum))
				toggleButtonMap(slotNum) = true
			  }
			} else {
			  error("PageCommander.toggleButton called for slotNum " + slotNum + ", but no entry found in toggleButtonMap")
			}
		  } else {
			error("PageCommander.toggleButton called for slotNum " + slotNum + ", but no TOGGLEBUTTON found in controlDefMap")
		  }
		} else {
		  error("PageCommander.toggleButton called for slotNum " + slotNum + ", but no entry found in controlDefMap")
		}
		success
	  }
	  
	  def outputSpeech(text: String) {
		outputSpeech = text
		updateInfo = 203 // To tell JavaScriptActor we want Android devices to say the text
		updateListeners()
	  }
	  
	  def requestContinuousSpeech(slotNum: Int, desired: Boolean) {
		info("In requestContinuousSpeech, setting to " + desired)
		if (desired) {
		  lastSpeechReqSlotNum = slotNum
		  updateInfo = 204 // To request start command - Needs to become a named constant soon
		  updateListeners()
		} else {
		  updateInfo = 205 // To request stop command - Needs to become a named constant soon
		  updateListeners()
		}
	  }
	  
	  def getSpeechReqControl = lastSpeechReqSlotNum
	  
	  def getOutputSpeech = {
		outputSpeech
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

