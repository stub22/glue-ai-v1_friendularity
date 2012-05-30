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

	object ListBox {
	  
	  val responseText = "Title can change" // We can add bits to define this in XML if we want, or code in more fancy conditionals (Currently ignored here for demo purposes)
  
	  val titlePrefix = "listformtitle"
	  val boxId = "listbox"
	  val blankId: Int = -1
	  
	  val titleMap = scala.collection.mutable.HashMap(blankId -> "No title text found") // Map to hold the titles for each form handled by this snippet, plus default
	  val labelMap = new scala.collection.mutable.HashMap[Int,List[String]] // Map to hold all the labels for each ListBox control rendered
	  
	  def makeListBox(labelText: String, labelList:List[String], idNum: Int): NodeSeq = {
		titleMap(idNum) = labelText
		labelMap(idNum) = labelList
		val formIdForHtml: String = idNum.toString
		val titleId: String = titlePrefix + formIdForHtml // We need a unique ID here, because JavaScript may be updating the title after post
		(
		  <form class='lift:form.ajax'>
			<lift:ListBox formId={formIdForHtml}>
			  <div id={titleId} class='labels'></div>
			  <input id={boxId} class='formlabels'/>
			</lift:ListBox>
		  </form>
		)
	  }
	}
	  
	class ListBox extends StatefulSnippet {
		
	  var formId: Int = ListBox.blankId
	  lazy val listBoxInstanceTitle = ListBox.titlePrefix + formId
		
	  def dispatch = {case "render" => render}
		
	  def render(xhtml:NodeSeq): NodeSeq = {

		def process(result: String): JsCmd = {
		  println("ListBox says option number " + result + " on formId " + formId + " is selected.")
		  //SetHtml(listBoxInstanceTitle, Text(ListBox.responseText)) // We'll leave the title the same for the demo
		  PageCommander.controlActionMapper(formId, result.toInt)
		}

		formId = (S.attr("formId") openOr "-1").toInt
		var valid = false
		var selectors:CssSel = "i_eat_yaks_for_breakfast" #> "" // This is just to produce a "Null" CssSel so we can initialize this here, but not add any meaningful info until we have checked for valid formId. (As recommended by the inventor of Lift)
		if (ListBox.titleMap.contains(formId)) {
		  valid = true
		  val titleSelectorText: String = "#"+listBoxInstanceTitle+" *"
		  val boxSelectorText: String = "#" + ListBox.boxId
		  val rows = if (ListBox.labelMap(formId).length < 8) ListBox.labelMap(formId).length else 7
		  val listPairs = (for (i <- 0 until ListBox.labelMap(formId).length) yield (i.toString, ListBox.labelMap(formId)(i)))// There may be a simplier Scala way to do this
		  selectors = titleSelectorText #> ListBox.titleMap(formId) & boxSelectorText #> SHtml.ajaxSelect(listPairs, Empty, process _, "class" -> "formlabels", "size" -> rows.toString)
		} else println("ListBox.render cannot find a valid formId! Reported formId: " + formId) //; NodeSeq.Empty
		if (valid) selectors.apply(xhtml) else NodeSeq.Empty // Blanks control if something is wrong with formId
		//selectors.apply(xhtml) // This would be ok too, and would just apply the "null" selector transform to html if something is broken
	  }
	}

  }
}
