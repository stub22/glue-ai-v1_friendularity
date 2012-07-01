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
	
	object TextForm {
	  val defaultText = "" // We can add bits to define this in XML if we want
	  //val responseText = "Thanks for the input!" // We can add bits to define this in XML if we want - will probably do so soon, but disabling for "operational" demo right now
	  val afterEntryText = "" // Right now we just clear text after input; we can do whatever we want
	  val submitLabel = "Submit" // We can add bits to define this in XML if we want
	  val textBoxRows = 7;
	  
	  val labelIdPrefix = "textformlabel"
	  val textBoxIdPrefix = "text_in"
	  val blankId: Int = -1
	  val textMap = scala.collection.mutable.HashMap(blankId -> "No label text found") // Map to hold the labels for each form handled by this snippet, plus default
	  
	  def makeTextForm(initialText: String, idNum: Int): NodeSeq = {
		textMap(idNum) = initialText
		val formIdforHtml: String = idNum.toString
		val labelId: String = labelIdPrefix + formIdforHtml // We need a unique ID here, because JavaScript will be updating the label after post
		val inputId: String = textBoxIdPrefix + formIdforHtml // JavaScript may want to do things to the input box too, like clear it
		// For good form and designer-friendliness, it would be nice to have all the XML in a template. But, we need to generate it here in order to set attributes. Maybe I can find a better way eventually.
		<form class="lift:form.ajax"><lift:TextForm formId={formIdforHtml}><div class="labels" id={labelId}></div><input id={inputId}/> <input type="submit" value={submitLabel}/></lift:TextForm></form>
	  }
	}

	class TextForm extends StatefulSnippet {
	  var text: String = TextForm.defaultText
	  var formId: Int = TextForm.blankId
	  lazy val textFormInstanceLabel = TextForm.labelIdPrefix + formId
	  lazy val textBoxInstanceLabel = TextForm.textBoxIdPrefix + formId
	 
	  def dispatch = {case "render" => render}	  
	  
	  def render = {
   
		def process(): JsCmd = {
		  println("Input text for form #" + formId + ": " + text)
		  val processThread = new Thread(new Runnable { // A new thread to call back into PageCommander to make sure we don't block Ajax handling
			  def run() {
				PageCommander.textInputMapper(formId, text) // Let PageCommander know about the text so it can figure out what to do with it
			  }
			})
		  processThread.start
		  //SetHtml(textFormInstanceLabel, Text(TextForm.responseText)) & // for now, this is disabled for the "operational" demo requirements
		  SetValById(textBoxInstanceLabel, TextForm.afterEntryText)
		}
		
		formId = (S.attr("formId") openOr "-1").toInt
		val labelSelectorText: String = "#"+textFormInstanceLabel+" *"
		val boxSelectorText: String = "#"+textBoxInstanceLabel
		labelSelectorText #> TextForm.textMap(formId) &
		boxSelectorText #> (SHtml.textarea(text, text = _, "rows" -> TextForm.textBoxRows.toString, "id" -> textBoxInstanceLabel) ++ SHtml.hidden(process))
	  }
	}

  }
}
