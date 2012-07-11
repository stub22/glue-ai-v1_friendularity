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

	  private var controlDefMap = new scala.collection.mutable.HashMap[Int, ControlConfig]
	  private var controlsMap = new scala.collection.mutable.HashMap[Int, NodeSeq]
	  private var singularAction = new scala.collection.mutable.HashMap[Int, String] // Holds action for currently enabled state of a multi-state control, such as a TOGGLEBUTTON
	  
	  // These guys hold lists of slotNums which will display text from Cogbot, or from Android speech input
	  private var cogbotDisplayers = new scala.collection.mutable.ArrayBuffer[Int]
	  private var speechDisplayers = new scala.collection.mutable.ArrayBuffer[Int]
	  // ... this one for ToggleButton states
	  private var toggleButtonMap =  new scala.collection.mutable.HashMap[Int, Boolean]
	  
	  private var appVariablesMap = new scala.collection.mutable.HashMap[String, String] // A place to hold variables that can be defined and set dynamically by the apps defined in the lift config files themselves
	  
	  private var requestedPage: Option[String] = None // A variable to hold the path to a page requested by LiftAmbassador
	  private var outputSpeech: String = "" // Holds speech we want Android to say
	  private var lastSpeechReqSlotNum = 0 // slotNum of last control which requested speech - used by JavaScriptActor to add identifying info to request
	  private var cogbotSpeaks = false // Determines whether Cogbot speech out also triggers Android speech
	  private var currentTemplate = "12slots" // Name of current template (in /templates-hidden) which corresponds to current liftConfig
	  
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
		toggleButtonMap.clear
		singularAction.clear
		appVariablesMap.clear // Probably we won't want this to clear permanently. It's sort of a quick-fix for now to make sure that toggle button states don't get out of sync with app variables they control when a "page" is exited and reentered
		
		val liftConfig = LiftAmbassador.getConfig();
		val controlList: java.util.List[ControlConfig] = liftConfig.myCCs
		
		//Get current template and request it be set
		currentTemplate = liftConfig.template
		updateInfo = 301 // Special code to trigger TemplateActor
		updateListeners;
		
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
			
			controlDefMap(slotNum) = controlDef; //Save the controlDef for this slotNum for future reference
			
			controlType match {
			  case ControlType.PUSHYBUTTON => {
				  setControl(slotNum, PushyButton.makeButton(text, style, resource, slotNum))
				  // Check for "local" actions which PageCommander needs to handle, such as text display
				  initLocalActions(slotNum, action) // this method will modify action as necessary according to prefixes
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
		  })
		// Blank unspecified slots (out to 20)
		for (slot <- 1 to 20) {
		  if (!(controlDefMap contains slot)) {
			setControl(slot, NodeSeq.Empty)
		  }
		}
	  }
					  
	  def setControl(slotNum: Int, slotHtml: NodeSeq) {
		//info("Updating listeners in setControl: Control Slot #" + slotNum)
		controlsMap(slotNum) = slotHtml 
		updateInfo = slotNum
		updateListeners()
	  }
	  
	  // Check to see if any action requested requires PageCommander to do some local handling
	  def initLocalActions(slotNum:Int, action:String) {
		val splitAction = action.split("_")
		splitAction(0) match {
		  case ActionStrings.showText => {splitAction(1) match {
				case ActionStrings.COGBOT_TOKEN => cogbotDisplayers += slotNum // Show Cogbot speech on this control? Add it to the cogbotDisplayers list.
				case ActionStrings.ANDROID_SPEECH_TOKEN => speechDisplayers += slotNum // Add to the speechDisplayers list if we want Android speech shown here
				case _ => warn("checkLocalActions doesn't know what to do in order to display text with token " + splitAction(1))
			  }
			}			  
		  case _ => // looks like this action doesn't require anything to happen locally, so do nothing
		}
	  }

	  
	  // A central place to define actions performed by displayed controls - may want to move to its own class eventually
	  def controlActionMapper(formId:Int, subControl:Int) {
		val splitAction = controlDefMap(formId).action.split("_")
		splitAction(0) match {
		  case ActionStrings.setVariable => {
			  //val textItems = List.fromArray(text.split(",")) //not sure the List.fromArray is necessary
			  val textItems = List.fromArray(controlDefMap(formId).text.split(","))
			  val textIndex = subControl + 1
			  appVariablesMap(splitAction(1)) = textItems(textIndex)
			  info("App Variable " + splitAction(1) + " set to " + textItems(textIndex))
			}
		  case ActionStrings.oldDemo => { // Just a way to include the old hard-coded demo just a little longer; soon will configure all of this from RDF
			  subControl match { // An early hard coded demo - must move to RDF definition very soon!!
				case 0 => setControl(6, PushyButton.makeButton("A button", "buttonred", "", 6))
				case 1 => setControl(6, TextForm.makeTextForm("A text box", 6))
				case 2 => setControl(6, SelectBoxes.makeSelectBoxes("Checkboxes", List("an option", "and another"), 6))
				case 3 => setControl(6, RadioButtons.makeRadioButtons("Radio buttons", List("Radio Option 1", "Radio Option 2"), 6))
				case _ =>
			  }
			}
		  case _ =>
		}
	  }
	  
	  // Similarly, a central place to handle text input.
	  def textInputMapper(formId:Int, text:String) {
		var desiredAction = controlDefMap(formId).action
		if (singularAction contains formId) {desiredAction = singularAction(formId)} // If this is a "multi-state" control, get the action corresponding to current state
		// If we see an action with a get speech command, set displayInputSpeech, strip off acquire command and see what's behind
		var displayInputSpeech = false;
		if (desiredAction startsWith ActionStrings.acquireSpeech) {
		  desiredAction = desiredAction.stripPrefix(ActionStrings.acquireSpeech + "_")
		  displayInputSpeech = true;
		} else if (desiredAction startsWith ActionStrings.getContinuousSpeech) {
		  desiredAction = desiredAction.stripPrefix(ActionStrings.getContinuousSpeech + "_")
		  displayInputSpeech = true;
		}
		//info("In textInputMapper; desiredAction is " + desiredAction) // TEST ONLY
		if (displayInputSpeech) {
		  speechDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("I think you said \"" + text + "\"", controlDefMap(slotNum).style, "", slotNum)))
		} // ... then continue to see if RDF tells us we need to do anything else with speech
		if (desiredAction.startsWith(ActionStrings.submitText)) { //... otherwise, we don't have a properly defined action for this text input
		  val stringToStrip = ActionStrings.submitText + "_"
		  val actionToken = desiredAction.stripPrefix(stringToStrip)
		  actionToken match {
			case ActionStrings.COGBOT_TOKEN => {
				if (cogbotDisplayers != Nil) { // Likely this check is not necessary - foreach just won't execute if list is Nil, right?
				  val response = LiftAmbassador.getCogbotResponse(text)
				  val cleanedResponse = response.replaceAll("<.*>", ""); // For now, things are more readable if we just discard embedded XML
				  cogbotDisplayers.foreach(slotNum => setControl(slotNum, PushyButton.makeButton("Cogbot said \"" + cleanedResponse + "\"", controlDefMap(slotNum).style, "", slotNum)))
				  if (cogbotSpeaks) outputSpeech(cleanedResponse) // Output Android speech if cogbotSpeaks is set
				}
			  }
			case _ => {
				// Send text to LiftAmbassador, see if it knows what to do with it
				if (!LiftAmbassador.sendTextToCogChar(actionToken, text)) {
				  warn("No action found in textInputMapper for token " + actionToken)
				}
			  }
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
			// In case this is a scene and Cog Char tells us to show the info page, be sure it has the required info 
			setSceneRunningInfo(id) // eventually this may not be necessary, and Cog Char may handle this part too}
			success = LiftAmbassador.triggerAction(action)
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
		val splitAction = action.split("_")
		splitAction(0) match {
		  case ActionStrings.acquireSpeech => {
			  updateInfo = 201 // Special "slotNum" to tell JavaScriptActor to request speech
			  lastSpeechReqSlotNum = slotNum; // Set this field - JavaScriptActor will use it to attach requesting info to JS Call - allows multiple speech request controls
			  updateListeners()
			  success = true
			}
		  case ActionStrings.cogbotSpeech => splitAction(1) match {
			  case ActionStrings.ENABLE_TOKEN => cogbotSpeaks = true; success = true
			  case ActionStrings.DISABLE_TOKEN => cogbotSpeaks = false; success = true
			  case _ => // No match, just exit (success=false)
			}
		  case ActionStrings.getContinuousSpeech => {
			  requestContinuousSpeech(slotNum, true)
			}
		  case ActionStrings.stopContinuousSpeech => {
			  requestContinuousSpeech(slotNum, false)
			}
		  case ActionStrings.setVariable => {
			  //A button wants to set a variable. That means we toggle the value between true and false.
			  if (toggleButtonMap contains slotNum) { // ... make sure the value is synced with the button state if so
				appVariablesMap(splitAction(1)) = toggleButtonMap(slotNum).toString 
			  } else if (appVariablesMap contains splitAction(1)) {
				if (appVariablesMap(splitAction(1)).toBoolean) appVariablesMap(splitAction(1)) = false.toString else appVariablesMap(splitAction(1)) = true.toString
			  } else appVariablesMap(splitAction(1)) = true.toString
			  info("App Variable " + splitAction(1) + " set to " + appVariablesMap(splitAction(1)))
			}
		  case _ => // No match, just exit (success=false)
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
				// If only one parameter is specified in RDF, duplicate the first and use that parameter here too (really we are prepending the one item in the list to itself, but that works ok here)
				if (actionItems.length < 2) actionItems ::= actionItems(0)
				singularAction(slotNum) = actionItems(1)
				toggleButtonMap(slotNum) = false
				success = continueTriggering(slotNum)
				setControl(slotNum, PushyButton.makeButton(textItems(0), styleItems(0), resourceItems(0), slotNum))
			  } else {
				// Button is set as "default" -- set to "selected" and perform action
				// If only one parameter is specified in RDF, duplicate the first and use that parameter here too (really we are prepending the one item in the list to itself, but that works ok here)
				if (textItems.length < 2) textItems ::= textItems(0)
				if (styleItems.length < 2) styleItems ::= styleItems(0)
				if (resourceItems.length < 2) resourceItems ::= resourceItems(0)
				singularAction(slotNum) = actionItems(0)
				toggleButtonMap(slotNum) = true
				success = continueTriggering(slotNum)
				setControl(slotNum, PushyButton.makeButton(textItems(1), styleItems(1), resourceItems(1), slotNum))
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
	  
	  def getCurrentTemplate = currentTemplate
	  
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
		def getVariable(key:String): String = {
		  var contents:String = null
		  if (appVariablesMap contains key) contents = appVariablesMap(key)
		  contents
		}
	  }
	
	}

  }
}

