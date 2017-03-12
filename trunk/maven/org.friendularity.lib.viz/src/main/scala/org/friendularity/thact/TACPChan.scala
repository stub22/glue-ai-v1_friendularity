package org.friendularity.thact

import org.friendularity.infra.cpump.{CPumpAdptr, CPChanPost, CPChanListen, CPumpCtx}

/**
  * Created by Owner on 4/7/2016.
  */
trait TACPChanListen[CtxType <: CPumpCtx] extends CPChanListen[CPThingActionMsg, CtxType] {

}

trait TACPChanPost[CtxType <: CPumpCtx] extends CPChanPost[CPThingActionMsg, CtxType] {

}

trait TACPFilterAdptr[CtxType <: CPumpCtx] extends CPumpAdptr[CPThingActionMsg, CtxType, CPThingActionMsg] {

}

