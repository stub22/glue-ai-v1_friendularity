package org.friendularity.cpump

import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Owner on 3/24/2016.
  */
// Remember that Adptrs are stateless and java-serializable.
// They may be transparently copied into another process.
// "Filter" indicates same formal input and output type.
trait DullFilterAdptr[FMsgType <: CPumpMsg] extends CPumpAdptr[FMsgType, DullPumpCtx, FMsgType] with VarargsLogging {


	override protected def attemptShortcut(inMsg: FMsgType, pumpCtx: DullPumpCtx): Traversable[FMsgType] = Nil
	override protected def mapIn(inMsg: FMsgType, ctx: DullPumpCtx): Traversable[WritableRecord] = Nil
	override protected def mapOut(inMsg: FMsgType, wresults : Traversable[WrittenResult], pumpCtx: DullPumpCtx): Traversable[FMsgType] = Nil
	override protected def write(rec: WritableRecord, wc: WritingCtx): WrittenResult = null
	override def processMsg(inMsg : FMsgType, pumpCtx : DullPumpCtx) : Traversable[FMsgType] = {
		info2("DMAdptrBase.processMsg msg={} adptr={}", inMsg, this)
		super.processMsg(inMsg, pumpCtx)
	}

	override def getLegalCtxClz: Class[DullPumpCtx] = classOf[DullPumpCtx]

	protected def getFilterMsgClz : Class[FMsgType]

	override def getLegalInMsgClz = getFilterMsgClz
	override def getLegalOutMsgClz = getFilterMsgClz
	override def getUsualInMsgClz  = getFilterMsgClz  // Subtype may override to any subclass of commonMsgClz

}
case class TxtDullFilterAdptr(filterExpr : String) extends DullFilterAdptr[TxtSymMsg]  {

	override def getFilterMsgClz = classOf[TxtSymMsg]

}