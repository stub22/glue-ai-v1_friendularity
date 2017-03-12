package org.friendularity.infra.mbm

import akka.actor.Actor
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.{CPumpMsg, CPStrongTeller}
import org.friendularity.infra.field.ReportingTickChance
import org.friendularity.infra.raiz.{VizappProfileLoader, VizappProfileLoaderImpl, VizappProfileRoots}

import scala.collection.mutable

/**
  * Created by Owner on 3/7/2017.
  */

trait MBMProfMgr extends VarargsLogging {

	// Input None when we expect only one loader and one profile, fully specified by loader's roots.
	// Used to search through multiple loaders for first match, overriding tokens in loader's roots
	def findValidProfile(ovrlTok_opt : Option[Array[String]]) : Option[JenaModel] = {
		val ldrs = getAllLoaders
		// TODO:  Once we find a valid (nonempty) profile, we can stop trying additional loaders.
		val profs : List[JenaModel] = ldrs.flatMap( vpl => {
			val pl: VizappProfileLoader = vpl
			val jm_opt = pl.makeMrgPrfGrphFrsh(ovrlTok_opt)
			jm_opt.toList
		})
		if (profs.size == 1) {
			info1("Found 1 valid profile graph as expected for ovl toks: {}", ovrlTok_opt)
		}
		if (profs.size != 1) {
			warn2("*********** OOOPS ***** Found profile graphs in unexpected {} loaders for ovl toks: {}", profs.size : Integer, ovrlTok_opt )
		}
		profs.headOption
	}

	protected def getAllLoaders : List[VizappProfileLoader]
}
trait MBMProfLoaderAdmin {
	def clearLoaders : Unit
	def appendLoader(pl : VizappProfileLoader) : Unit
	def prependLoader(pl : VizappProfileLoader) : Unit

	def appendLoader(pr : VizappProfileRoots) : Unit
	def prependLoader(pr : VizappProfileRoots) : Unit

	// def getAllLoaders : List[VizappProfileLoader]
}

trait MBMProfMgrImpl extends MBMProfMgr with  MBMProfLoaderAdmin {
	private var myLoaders : List[VizappProfileLoader] = Nil
	private def makeLoader(pr : VizappProfileRoots) : VizappProfileLoader = {
		new VizappProfileLoaderImpl(pr)
	}
	override def clearLoaders : Unit = {
		myLoaders = Nil
		debug1("myLoaders is now: {}", myLoaders)
	}
	override def appendLoader(pl : VizappProfileLoader) : Unit = {
		myLoaders = myLoaders ::: List(pl)
		debug1("myLoaders is now: {}", myLoaders)
	}
	override def appendLoader(pr : VizappProfileRoots) : Unit = appendLoader(makeLoader(pr))

	override def prependLoader(pl : VizappProfileLoader) : Unit = {
		myLoaders = List(pl) ::: myLoaders
		debug1("myLoaders is now: {}", myLoaders)
	}
	def prependLoader(pr : VizappProfileRoots) : Unit = prependLoader(makeLoader(pr))

	override protected def getAllLoaders : List[VizappProfileLoader] = myLoaders

}

trait PLMsg extends CPumpMsg
trait PLRqst extends PLMsg
trait PLRslt extends PLMsg
// 3 messages allow managing the list of available loaders, in order.
case class ClearLoaders() extends PLRqst
case class AppendLoader(pr : VizappProfileRoots) extends PLRqst
case class PrependLoader(pr : VizappProfileRoots) extends PLRqst

case class ProfileResult(jmOpt : Option[JenaModel], origRq : RequestProfileGraph) extends PLRslt

case class RequestProfileGraph(ovrlTok_opt : Option[Array[String]],
							   tellMe : CPStrongTeller[ProfileResult]) extends PLRqst

trait PLRqstPrcsr extends VarargsLogging {
	protected def getLoaderAdmin : MBMProfLoaderAdmin

	protected def getProfMgr : MBMProfMgr

	def processRqst(plrq : PLRqst) : Unit = {
		debug1("Processing plRqst: {}", plrq)
		plrq match {
			case rqCL: ClearLoaders => {
				getLoaderAdmin.clearLoaders
			}
			case rqAL: AppendLoader => {
				getLoaderAdmin.appendLoader(rqAL.pr)
			}
			case rqPL: PrependLoader => {
				getLoaderAdmin.prependLoader(rqPL.pr)
			}
			case rqRPG: RequestProfileGraph => {
				val pgraph_opt: Option[JenaModel] = getProfMgr.findValidProfile(rqRPG.ovrlTok_opt)
				val rslt = new ProfileResult(pgraph_opt, rqRPG)
				rqRPG.tellMe.tellStrongCPMsg(rslt)
			}
			case other : Any => {
				error1("Received unexpected profile request, ignoring: {}", other)
			}
		}
	}
}

// Extend this trait on the client side to asynchronously consume the "best" profile for given
// tokens, when it becomes available.
trait ProfileLoadJob {
	// Override this method to specify the important tokens (all are required)
	def getOvlTokens : Set[String] = Set()

	// Override this method to (asynchronously) receive the result profile model
	def consumeRslt (rmo : Option[JenaModel]) : Unit
}

// Server actor
class MBMProfMgrActr extends PLRqstPrcsr with Actor {
	lazy val myInternalImpl = new  MBMProfMgrImpl{}
	override protected def getLoaderAdmin: MBMProfLoaderAdmin = myInternalImpl
	override protected def getProfMgr: MBMProfMgr = myInternalImpl

	override def receive : Actor.Receive = {
		case plRqst : PLRqst => {
			processRqst(plRqst)
		}
	}

}
trait BootState

trait PrelimBootState extends BootState
private case class BS_UNREAD() extends PrelimBootState
private case class BS_READ_PROF() extends PrelimBootState
private case class BS_HAS_PROF() extends PrelimBootState

// During these three states, system may do unique things
trait WorkingBootState extends BootState
private case class BS_BOOTING() extends WorkingBootState
private case class BS_RUNNING() extends WorkingBootState
private case class BS_STOPPING() extends WorkingBootState

trait FinalBootState extends BootState
private case class BS_STOPPED() extends FinalBootState
private case class BS_FAILED_LOAD() extends FinalBootState
private case class BS_FAILED_BOOT() extends FinalBootState
private case class BS_FAILED_RUN() extends FinalBootState

trait HasBootState {
	private var myCurrBS : BootState = new BS_UNREAD
}
trait BootableOnce extends HasBootState with ProfileLoadJob {

}
trait BOImpl extends BootableOnce {

}
class BootSlot(private val mySysID : Ident) {
	var myRebootCount : Int = 0
	// var myActiveBootable_opt : Option[BootableOnce] = None
	var myBootablesChronOrder : List[BootableOnce] = Nil
	// Returns an index number for the attempt, or -1 on immediate failure.
	def startRebootAttempt(ovlToks_opt : Option[Array[String]]) : Int = {
		synchronized {
			myRebootCount += 1

			myRebootCount
		}
	}
}
trait BootingSystemSet {
	val mySysByID = new mutable.HashMap[Ident, BootableOnce]
	//
	def startReboot(sysID : Ident, ovlToks_opt : Option[Array[String]]) : Int = {
		0
	}
	def checkStatus(sysID : Ident)
}
trait BSSImpl extends BootingSystemSet {

}