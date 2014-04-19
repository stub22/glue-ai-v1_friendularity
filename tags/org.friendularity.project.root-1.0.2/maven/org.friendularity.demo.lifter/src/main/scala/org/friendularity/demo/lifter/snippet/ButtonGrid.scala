package org.friendularity.demo.lifter {
package snippet {


import net.liftweb.common._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._
import S._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.util._
import Helpers._
import _root_.java.util.Date
import scala.xml._


object ButtonGrid {

def now = new Date()

  def render1 ={
    "#button1 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 1 was pressed at " + now)
      Call("button1_set", "") &
      Call("CSSswitch", "")})   
}

  def render2 ={

    "#button2 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 2 was pressed at " + now)
      Call("button2_set", "") &
      Call("CSSswitch", "")})   
}
   def render3 ={
   "#button3 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 3 was pressed at " + now)
      Call("button3_set", "") &
      Call("CSSswitch", "")})   
}
   def render4 ={   
"#button4 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 4 was pressed at " + now)
      Call("button4_set", "") &
      Call("CSSswitch", "")})   
}

   def render5 ={   
"#button5 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 5 was pressed at " + now)
      Call("button5_set", "") &
      Call("CSSswitch", "")})   
}
 
   def render6 ={   
"#button6 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 6 was pressed at " + now)
      Call("button6_set", "") &
      Call("CSSswitch", "")})   
}

    def render7 ={
    "#button7 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 7 was pressed at " + now)
      Call("button7_set", "") &
      Call("CSSswitch", "")})   
}
 def render8 ={
   "#button8 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 8 was pressed at " + now)
      Call("button8_set", "") &
      Call("CSSswitch", "")})   
}
 def render9 ={
   "#button9 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 9 was pressed at " + now)
      Call("button9_set", "") &
      Call("CSSswitch", "")})   
}
 
def render10 ={   
"#button10 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 10 was pressed at " + now)
      Call("button10_set", "") &
      Call("CSSswitch", "")})   
}

 def render11 ={  
"#button11 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 11 was pressed at " + now)
      Call("button11_set", "") &
      Call("CSSswitch", "")})
}
 def render12 ={   "#button12 [onclick]" #> SHtml.ajaxInvoke (() => {
      println("Button 12 was pressed at " + now)
      Call("button12_set", "") &
      Call("CSSswitch", "")})
  }
}
}
}
