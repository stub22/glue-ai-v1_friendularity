package org.friendularity.vw.msg.cor

/**
  * Created by Owner on 1/18/2017.
  */
trait VWNoticeCor // is just the name of the Scala file.

// Notice => Outbound from something
trait VWorldNotice extends VWorldMsg

// Outbound from VWorld inner core, i.e. the magic launch state.
// "Internal" => Does not go out to app logic or over QPid topics, usually.
trait VWorldInternalNotice extends  VWorldNotice
