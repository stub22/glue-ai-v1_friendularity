package org.friendularity.vw.msg.ovl

import org.friendularity.vw.impl.ovl.OverlayPage
import org.friendularity.vw.msg.cor.VWOverlayRq


/**
  * Created by Owner on 1/18/2017.
  */


case class VWSetupOvlBookRq(pages : List[OverlayPage]) extends VWOverlayRq