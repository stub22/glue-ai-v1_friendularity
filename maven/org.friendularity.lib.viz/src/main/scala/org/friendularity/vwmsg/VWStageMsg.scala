package org.friendularity.vwmsg

import com.jme3.math.ColorRGBA
import org.appdapter.core.name.Ident
import org.friendularity.cpmsg.CPMsgTeller
import org.friendularity.vwimpl.OverlayPage
import com.jme3.math.Vector3f

/**
  * Created by Stub22 on 6/16/2016.
  */
// Used for messages about lights, cameras and related stagecraft - also messages for input keymap
trait VWStageRqMsg extends VWorldRequest

case class VWStageResetToDefault() extends VWStageRqMsg // Reset all camera + lighting effects to stored defaults
case class VWStageEmulateBonusContentAndCams() extends VWStageRqMsg

case class VWStageOpticsBasic(location : Vector3f, direction :Vector3f, moveSpeed : Int, pauseOnLostFocus : Boolean, dragMouseToRotateCamera: Boolean)  extends VWStageRqMsg
case class VWStageBackgroundColor(bgColor: ColorRGBA)  extends VWStageRqMsg
case class VWStageBackgroundSkybox(northImagePath: String, eastImagePath: String, southImagePath: String,
				  westImagePath: String, upImagePath: String, downImagePath: String)  extends VWStageRqMsg

/**
* @param displayContentStatsOnScreen FrameBuffers, Textures, Shaders, Objects, etc..
* @param displayFPSOnScreen Show frames per second
*/
case class VWStatsViewMessage(displayContentStatsOnScreen: Boolean, displayFPSOnScreen: Boolean)  extends VWStageRqMsg
case class VWStageSetupLighting(ambientLightColor: ColorRGBA)  extends VWStageRqMsg

// JME describes the coords as:   float left, float right, float bottom, float top) {
case class ViewportDesc(myX1_left : Float, myX2_right : Float, myY1_bot : Float, myY2_top : Float,
						myBGColor_opt : Option[ColorRGBA])

// initXform is for the camera itself, and is in world coordinates.
// Initially the camera is *not* the child of a parentNode, and thus does not have any local-coords notion.
// Camera does not use scale xform, just pos and point.

// Q: What happens to this xform if we Bind it, and then manipulate direction by *both* methods?
case class VWCreateCamAndViewportRq(camID : Ident, initState: CamState3D, initVP : ViewportDesc) extends VWStageRqMsg

// Again with the global xform state.
case class VWModifyCamStateRq(camID : Ident, updState_opt : Option[CamState3D], updVP_opt: Option[ViewportDesc]) extends VWStageRqMsg

// Must refer to an existing cam, which was either made by VWCreateCam_, or is known by some wellKnownID.
// node will be attached as camera parent or child depending on flag.
// Shape(node) is an example of a spaceNode.
case class VWBindCamNodeRq(camID : Ident, spaceNodeIsCamParent_NotChild : Boolean, spaceTeller : CPMsgTeller, spaceNodeID : Ident) extends VWStageRqMsg

// This can
// case class VWCamNodeManipRq(camNodeID : Ident, manipGuts : ManipDesc) extends VWStageRqMsg



