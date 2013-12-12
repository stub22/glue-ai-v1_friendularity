package org.friendularity.demo.lifter {
package comet {


import net.liftweb.common._
import net.liftweb.http.js.JE._
import net.liftweb.http._
import S._
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.util._
import Helpers._
import _root_.java.util.Date
import scala.xml._
import scala.util.Random


class PushActor extends CometActor {
  override def defaultPrefix = Full("clk")   
var r = new Random()
var PushSeconds = (r.nextInt(5)+3)*1000
    
      
  def render = bind("RandPush" -> RandPushSpan)

      
  def RandPushSpan = (<span id="RandPush">{PushSeconds}</span>)
  
 

  // redraw ping every 3-7 seconds
  ActorPing.schedule(this, Tick, 1000L) 

  override def lowPriority : PartialFunction[Any, Unit] = {
    case Tick => {
      partialUpdate(SetHtml("RandPush", Text((PushSeconds/1000).toString))) ;
      ActorPing.schedule(this, Tick, PushSeconds) ;
      PushSeconds = (r.nextInt(5)+3)*1000;
    }
  }
}
case object Tick
}
}