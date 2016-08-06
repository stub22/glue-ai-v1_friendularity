package org.friendularity.field

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.CPStrongTeller

import scala.collection.mutable.{HashMap => MutableHashMap}

/**
  * Created by Owner on 7/17/2016.
  */
trait MonitoredSpace {
	// if updateFilter_opt is None, chan will autoclose after initial send.
	def openReportingChan(chanID : Ident, toWhom : CPStrongTeller[SourceDataMsg], policy : ReportingPolicy)
	def closeReportingChan(chanID : Ident) = ???
	def suspendReportingChan(chanID : Ident) = ???
	def resumeReportingChan(chanID: Ident) = ???
}

case class ReportStream(chanID : Ident, toWhom : CPStrongTeller[SourceDataMsg], policy : ReportingPolicy) {
	def sendReport(fieldDatBrdthFrst : List[ItemFieldData]) : Unit = {
		val tstampMsec = System.currentTimeMillis()
		val msg = new SourceDataMsgImpl(tstampMsec, fieldDatBrdthFrst)
	}
}

trait MonitoredSpaceImpl extends MonitoredSpace with VarargsLogging {
	val myOpenReportStreams = new MutableHashMap[Ident, ReportStream]()
	override def openReportingChan(chanID : Ident, toWhom : CPStrongTeller[SourceDataMsg],
								   policy : ReportingPolicy): Unit = {
		if (!myOpenReportStreams.isDefinedAt(chanID)) {
			val strm = new ReportStream(chanID, toWhom, policy)
			val initFiltPolicy_opt = policy.initialFilter_opt
			if (initFiltPolicy_opt.isDefined) {
				val initReportFields = makeInitialReportFields(chanID, initFiltPolicy_opt.get)
				strm.sendReport(initReportFields)
			}
			if (policy.updateFilter_opt.isDefined) {
				myOpenReportStreams.put(chanID, strm)
			}
		} else 	{
			error2("Received request to open on EXISTING report stream with ID: {} to: {}", chanID, toWhom)
		}
	}
	def processSrcCtrlMsg(rscm : ReportSourceCtrlMsg) : Unit = {
		rscm match {
			case rso : ReportSrcOpen => {
				openReportingChan(rso.chanID, rso.toWhom, rso.policy)
			}
		}
	}

	def processTickReportingChance(rtc : ReportingTickChance) : Unit = {
		for (strm <- myOpenReportStreams.values) {
			val upPolicy = strm.policy.updateFilter_opt.get
			val upRepFields = makeTickUpdateReportFields(strm.chanID, upPolicy)
			if (upRepFields.nonEmpty) {
				strm.sendReport(upRepFields)
			}
		}
	}
	def processMsgToStatusSrc(msgToSrc : MsgToStatusSrc) : Unit = {
		msgToSrc match {
			case rscm :  ReportSourceCtrlMsg => {
				processSrcCtrlMsg(rscm)
			}
			case rtc : ReportingTickChance => {
				processTickReportingChance(rtc)
			}

		}
	}
	def makeInitialReportFields(chanID : Ident, filteringPolicy: ReportFilteringPolicy) : List[ItemFieldData] = {
		warn1("Default impl of sendInitialReport is sending empty initial report on chan={}", chanID)
		Nil
	}
	def makeTickUpdateReportFields(chanID : Ident, filteringPolicy: ReportFilteringPolicy) : List[ItemFieldData] = {
		Nil
	}
}
