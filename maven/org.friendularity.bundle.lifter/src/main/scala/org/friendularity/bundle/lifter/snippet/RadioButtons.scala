package org.friendularity.bundle.lifter {
  package snippet {

	import scala.xml._
	import net.liftweb._
	import http._
	import common._
	import js._
	import JsCmds._
	import JE._
	import net.liftweb.http.js.JsCmd
	import net.liftweb.util._
	import Helpers._
	import net.liftweb.http.SHtml._
	import org.friendularity.bundle.lifter.model.PageCommander
	import S._

	object RadioButtons extends Logger {
  
	  val responseText = "I see you!"
	  
	  val titlePrefix = "radiotitle"
	  val blankId: Int = -1
	  
	  val titleMap = scala.collection.mutable.HashMap(blankId -> "No title text found") // Map to hold the titles for each form handled by this snippet, plus default
	  val labelMap = new scala.collection.mutable.HashMap[Int,List[String]] // Map to hold all the labels for each RadioButtons control rendered

	  def makeRadioButtons(titleText: String, labelList:List[String], idNum: Int): NodeSeq = {
		titleMap(idNum) = titleText
		labelMap(idNum) = labelList
		val formIdForHtml: String = idNum.toString
		val titleId: String = titlePrefix + formIdForHtml // We need a unique ID here, because JavaScript will be updating the title after post
		<form class="lift:form.ajax"><lift:RadioButtons formId={formIdForHtml}><div class="labels" id={titleId}></div><div id="buttonshere"></div></lift:RadioButtons></form>
	  }  
	}
	
	class RadioButtons extends StatefulSnippet with Logger {
	  
	  var formId: Int = RadioButtons.blankId
	  lazy val radioButtonsInstanceTitleId = RadioButtons.titlePrefix + formId
	  
	  def dispatch = {case "render" => render}
	  
	  def render(xhtml:NodeSeq): NodeSeq = {
		
		def process(result: String): JsCmd = {
		  info("RadioButtons says option number " + result + " on formId " + formId + " is selected")
		  //SetHtml(radioButtonsInstanceTitleId, Text(RadioButtons.responseText)) //... or not for now
		  val processThread = new Thread(new Runnable { // A new thread to call back into PageCommander to make sure we don't block Ajax handling
			  def run() {
				PageCommander.controlActionMapper(formId, result.toInt)
			  }
			})
		  processThread.start
		  JsCmds.Noop
		}
		formId = (S.attr("formId") openOr "-1").toInt
		var valid = false
		var selectors:CssSel = "i_eat_yaks_for_breakfast" #> "" // This is just to produce a "Null" CssSel so we can initialize this here, but not add any meaningful info until we have checked for valid formId. (As recommended by the inventor of Lift)
		if (RadioButtons.titleMap.contains(formId)) {
		  valid = true
		  val titleSelectorText: String = "#"+radioButtonsInstanceTitleId+" *"
		  val buttonTags = (for (i <- 0 until RadioButtons.labelMap(formId).length) yield i.toString)// There may be a simplier Scala way to do this
		  val theButtons = SHtml.ajaxRadio(buttonTags, Empty, process _)
		  var buttonHtml: NodeSeq = NodeSeq.Empty
		  for (buttonIndex <- 0 until RadioButtons.labelMap(formId).length) {
			buttonHtml = buttonHtml ++ <div><span class="formlabels">{RadioButtons.labelMap(formId)(buttonIndex)}</span><span>{theButtons(buttonIndex)}</span></div>
		  }
		  selectors = titleSelectorText #> RadioButtons.titleMap(formId) & "#buttonshere" #> buttonHtml
		} else error("RadioButtons.render cannot find a valid formId! Reported formId: " + formId)
		if (valid) selectors.apply(xhtml) else NodeSeq.Empty // Blanks control if something is wrong with formId
		//selectors.apply(xhtml) // This would be ok too, and would just apply the "null" selector transform to html if something is broken
	  }  
	}

  }
}
