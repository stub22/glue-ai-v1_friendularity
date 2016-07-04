package org.friendularity.vwmsg

/**
  * Created by StuB22 on 6/21/2016.
  *
  * For keyboard events and the like.
  */


trait VWUserInputNotice extends VWorldNotice

// External callbacks should only send externaizable (VM-independent, String-encodable) messages at
case class VWKeymapBinding_Extern(inpNamesToActionNames : Map[Char,String]) extends VWStageRqMsg

// Medial = parallel an implemented in "OuterLogic".  Callbacks may send messages to internal, which may be nonserial in many cases
case class VWKeymapBinding_Medial(inpNamesToActionFuncs : Map[String,Function1[VWorldPublicTellers,Unit]],
								  pubTellers : VWorldPublicTellers) extends VWStageRqMsg

// We do not allow for a KeyBinding direct to Internal.  Stated another way:
// We do not allow for direct callback from keyboard into code that talks to the RenderRegistryClient, JME3, etc.
// That all must be done by actor messages triggered from the medial (or external) callback actions, responding to
// the messages above.

