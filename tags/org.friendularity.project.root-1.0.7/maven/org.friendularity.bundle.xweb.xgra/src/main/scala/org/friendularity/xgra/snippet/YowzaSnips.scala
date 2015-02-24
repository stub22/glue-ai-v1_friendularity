/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.xgra.snippet

import scala.xml.{NodeSeq,Text}
import net.liftweb.http.DispatchSnippet

/**
 * @author Stu B. <www.texpedient.com>
 */

class YowzaSnips {
	println("GGGGGGGGGGGGGGGGGGGG YowzaSnips instantiated")
	def wriggly(xhtml : NodeSeq) : NodeSeq = {
		println ("KKKKKKKKKKKKKKKKKKKKKKKKKKKKK wriggly invoked")
		Text("Yowza Wriggly Yowza")
	}
	def sniggly(xhtml : NodeSeq) : NodeSeq = {
		println ("JJJJJJJJJJJJJJJJJJJJ snriggly invoked")
		// Text("Yowza Sniggly Yowza")
		<span>Some <b>sniggly</b> deal</span>
	}
	def priggly(xhtml : NodeSeq) : NodeSeq = {
		println ("QQQQQQQQQQQQQQQQQQ priggly invoked")
		// Text("Yowza Priggly Yowza")
		QueryStuff.makeTableFromSparqlResults
	}
	
}
// "ParameterizedSparqlString is the correct approach for remote work.
import net.liftweb.http.{S, SHtml, LiftSession, RenderOut}
import net.liftweb.common.{Full, Box}
import net.liftweb.util.{Schedule, CssSel}
import net.liftweb.http.js.JsCmds.SetHtml

object FormTestSnips {
// render can have many forms equivalent to NodeSeq => NodeSeq (eager or lazy)
  def render() : CssSel  = {
    // define some closure variables to share state within a context (such as a page-view)
	var userName = "initial user name"
	var userAge = 0
	var subCount = 0;
   // define our submit callback WITHIN our rendered closure, so that it sees the enclosed state.
    def handleSubmitInsideRenderClosure() {
		subCount += 1
      // if the age is < 13, display an error
      if (userAge < 13) S.error("Too young! (from S.error) - subCount=" + subCount)
      else {
        // otherwise give the user feedback and
        // redirect to the home page
		// 
        S.notice("userName: "+ userName + "(from S.notice)")
        S.notice("userAge + 2: " + (userAge + 2))
		S.notice("submittedCount: " + subCount)
        S.redirectTo("/")
      }
    }
	
	//  http://scala-tools.org/mvnsites/liftweb-2.3/net/liftweb/util/ToCssBindPromoter.html
	// Below is CSS selector transform syntax for binding the input fields to operations attached to our closure varibles.	
	// The first string or other object on each line becomes a ToCssBindPromoter which gets called with one of
	// the 8 forms of #>
	 
	
	
	import net.liftweb.util.Helpers._
	// Note the ampersand chaining to yield a single output CssSel chain.
	/*
	trait CssSel extends (NodeSeq) => NodeSeq
This trait is both a NodeSeq => NodeSeq and has the ability to chain CssSel
instances so that they can be applied en masse to incoming NodeSeq and do the transformation.
*/
//// 
	// each transform binds an action taken onSubmit.   There are 5 forms of onSubmit in 
/*   SHtml, each of which accepts a kind of field-processing-func, which is meant to process 
 *   the data when the form is rendered later.
 *   
onSubmit(func: (String) => Any): (NodeSeq) => NodeSeq
onSubmitBoolean(func: (Boolean) => Any): (NodeSeq) => NodeSeq
onSubmitImpl(func: AFuncHolder): (NodeSeq) => NodeSeq
onSubmitList(func: (List[String]) => Any): (NodeSeq) => NodeSeq
onSubmitUnit(func: () => Any): (NodeSeq) => NodeSeq

		Appellido: <input width="80" fkey="appellido" /><br />
      Edad: <input value="4" fkey="edad" /><br />


 */
	
    val ourCssSel : CssSel =  
		"fkey=appellido" #> SHtml.onSubmit(userName = _) &     
		// set the age variable if we can convert to an Int
		"fkey=edad" #> SHtml.onSubmit(s => asInt(s).foreach(userAge = _)) &
		// when the form is submitted, call our 
		"type=submit" #> SHtml.onSubmitUnit(handleSubmitInsideRenderClosure)
		
	println("*******************************************************************************")
	println("*******************************************************************************")
		println("Made css-sel: " + ourCssSel)
		// Returning the css-sel gives same result (so far) as doing an apply ourselves on explicit input NodeSeq.
	//val ourOutNS : NodeSeq = ourCssSel.apply(inSeq)
	//	println("Made NodeSeq: " + ourOutNS)
	ourCssSel
  }
	
}