package org.friendularity.vwmsg

import com.jme3.math.ColorRGBA

/**
  * Created by Owner on 6/16/2016.
  */
// Used for messages about lights, cameras and related stagecraft
trait VWStageRqMsg extends VWorldRequest {
}

case class VWStageEmulateBonusContentAndCams() extends VWStageRqMsg


case class VWStageOpticsBasic(moveSpeed : Int, bgColor: ColorRGBA)  extends VWStageRqMsg

