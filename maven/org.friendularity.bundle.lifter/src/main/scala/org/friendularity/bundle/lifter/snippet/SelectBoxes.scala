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
	import org.friendularity.bundle.lifter.commander.PageCommander
	import S._

	object SelectBoxes {
	  
	  val responseText = "Title can change" // We can add bits to define this in XML if we want, or code in more fancy conditionals
  
	  val titlePrefix = "selectformtitle"
	  val labelPrefix = "label"
	  val boxPrefix = "checkbox"
	  val blankId: Int = -1
	  
	  val titleMap = scala.collection.mutable.HashMap(blankId -> "No title text found") // Map to hold the titles for each form handled by this snippet, plus default
	  val labelMap = new scala.collection.mutable.HashMap[Int,List[String]] // Map to hold all the labels for each SelectBoxes control rendered
	  
	  def makeSelectBoxes(labelText: String, labelList:List[String], idNum: Int): NodeSeq = {
		titleMap(idNum) = labelText
		labelMap(idNum) = labelList
		val formIdForHtml: String = idNum.toString
		val titleId: String = titlePrefix + formIdForHtml // We need a unique ID here, because JavaScript will be updating the title after post
		//var boxesHtmlString: String = "<form class=\"lift:form.ajax\"><lift:SelectBoxes formId=\"" + formIdForHtml +"\"><div id=\"" + titleId + "\" class=\"labels\">Label for form will appear here</div>" //the hard way
		var boxesHtmlString: String = "<form class='lift:form.ajax'><lift:SelectBoxes formId='" + formIdForHtml +"'><div id='" + titleId + "' class='labels'></div>"
		// Add html for each box
		for (boxIndex <- 0 until labelList.length) {
		  val labelId: String = labelPrefix + boxIndex.toString
		  val boxId: String = boxPrefix + boxIndex.toString
		  //boxesHtmlString += "<div><label for=\"" + boxId + "\" id=\"" + labelId + "\"></label><input id=\"" + boxId + "\"/></div>" //the hard way
		  boxesHtmlString += "<div><label class='formlabels' for='" + boxId + "' id='" + labelId + "'></label><input id='" + boxId + "'/></div>" //The CSS class for the labels is not being applied, I bet there's a simple reason why
		}
		boxesHtmlString += "</lift:SelectBoxes></form>"
		//println("Here's the string for SelectBox XML: " + boxesHtmlString) //DEBUGGING ONLY
		XML.loadString(boxesHtmlString)
	  }
	}
	  
	class SelectBoxes extends StatefulSnippet {
		
	  var formId: Int = SelectBoxes.blankId
	  lazy val selectBoxesInstanceTitle = SelectBoxes.titlePrefix + formId
		
	  def dispatch = {case "render" => render}
		
	  def render(xhtml:NodeSeq): NodeSeq = {

		def process(result: Boolean, boxNumber: Int): JsCmd = {
		  println("SelectBoxes says box number " + boxNumber + " on formId " + formId + " is " + result)
		  SetHtml(selectBoxesInstanceTitle, Text(SelectBoxes.responseText))
		}

		def makeABox(boxIndex:Int) = {
		  val labelId: String = "#" + SelectBoxes.labelPrefix + boxIndex.toString
		  val boxId: String = "#" + SelectBoxes.boxPrefix + boxIndex.toString
		  labelId #> SelectBoxes.labelMap(formId)(boxIndex) &
		  boxId #> SHtml.ajaxCheckbox(false, (toggled: Boolean) => process(toggled, boxIndex), "class" -> "showit")
		}

		formId = (S.attr("formId") openOr "-1").toInt
		var valid = false
		var selectors:CssSel = "i_eat_yaks_for_breakfast" #> "" // This is just to produce a "Null" CssSel so we can initialize this here, but not add any meaningful info until we have checked for valid formId. (As recommended by the inventor of Lift)
		if (SelectBoxes.titleMap.contains(formId)) {
		  valid = true
		  val titleSelectorText: String = "#"+selectBoxesInstanceTitle+" *"
		  selectors = titleSelectorText #> SelectBoxes.titleMap(formId)
		  for (boxIndex <- 0 until SelectBoxes.labelMap(formId).length) {
			selectors = selectors & makeABox(boxIndex)
		  }
		} else println("SelectBox.render cannot find a valid formId! Reported formId: " + formId)
		if (valid) selectors.apply(xhtml) else NodeSeq.Empty // Blanks control if something is wrong with formId
		//selectors.apply(xhtml) // This would be ok too, and would just apply the "null" selector transform to html if something is broken
	  }
	}

  }
}
