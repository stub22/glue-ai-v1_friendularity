package org.friendularity.vwmsg

import org.friendularity.vwimpl.MoreIsolatedBonusContentTask

/**
  * Created by Owner on 6/16/2016.
  */
// Used for messages about lights, cameras and related stagecraft
trait VWStageRqMsg extends VWorldRequest {
}

case class VWStageEmulateBonus() extends VWStageRqMsg {

}

