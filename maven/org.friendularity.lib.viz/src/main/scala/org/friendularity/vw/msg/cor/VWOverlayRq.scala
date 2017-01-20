package org.friendularity.vw.msg.cor

import org.friendularity.vw.impl.ovl.OverlayPage

/**
  * Created by Owner on 1/18/2017.
  */
trait VWOverlayRq extends VWorldRequest

case class VWSetupOvlBookRq(pages : List[OverlayPage]) extends VWOverlayRq