package org.friendularity.vwmsg

import com.jme3.math.ColorRGBA

/**
  * Created by Owner on 6/16/2016.
  */
// Used for messages about lights, cameras and related stagecraft
trait VWStageRqMsg extends VWorldRequest {
}


case class VWStageResetToDefault() extends VWStageRqMsg // Reset all camera + lighting effects to stored defaults
case class VWStageEmulateBonusContentAndCams() extends VWStageRqMsg

case class VWStageOpticsBasic(moveSpeed : Int, bgColor: ColorRGBA)  extends VWStageRqMsg

// External callbacks should only send externaizable (VM-independent, String-encodable) messages at
case class VWKeymapBinding_Extern(inpNamesToActionNames : Map[Char,String]) extends VWStageRqMsg

// Medial = parallel an implemented in "OuterLogic".  Callbacks may send messages to internal, which may be nonserial in many cases
case class VWKeymapBinding_Medial(inpNamesToActionFuncs : Map[String,Function1[VWorldPublicTellers,Unit]],
								  pubTellers : VWorldPublicTellers) extends VWStageRqMsg

// We do not allow for a KeyBinding direct to Internal.  Stated another way.
// We do not allow for direct callback from keyboard into code that talks to the RenderRegistryClient, JME3, etc.
// That all must be done by actor messages triggered from the medial (or external) callback actions, responding to
// the messages above.