package org.friendularity.vw.msg.intrn

import org.friendularity.vw.msg.cor.VWorldNotice

/**
  * Created by Owner on 2/18/2017.
  */
// Outbound from VWorld inner core, i.e. the magic launch state.
// "Internal" => Does not go out to app logic or over QPid topics, usually.
trait VWorldInternalNotice extends  VWorldNotice
