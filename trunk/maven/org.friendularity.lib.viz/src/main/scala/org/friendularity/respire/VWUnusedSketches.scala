package org.friendularity.respire

import java.util.{ArrayList, Map}
/*
import akka.actor.ActorContext
import com.hp.hpl.jena.rdf.model.Model
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.{EnhancedLocalRepoClient, RepoClient}
import org.cogchar.api.thing.WantsThingAction
import org.cogchar.app.puma.behavior.PumaBehaviorManager
import org.cogchar.app.puma.body.PumaDualBodyManager
import org.cogchar.app.puma.boot.{PumaContextCommandBox, PumaSysCtx}
import org.cogchar.app.puma.config.{BodyHandleRecord, PumaContextMediator}
import org.cogchar.app.puma.event.{Updater, CommandEvent}
import org.cogchar.app.puma.registry.{PumaRegistryClientImpl, PumaRegistryClient}
import org.cogchar.blob.entry.EntryHost
import org.cogchar.bundle.app.vworld.busker.TriggerItems
import org.cogchar.bundle.app.vworld.central.{VWCtxCmdBox, StatefulVWorldRegistry, VWorldRegistry}
import org.cogchar.platform.trigger.{CommandSpace, BoxSpace}
import org.friendularity.raiz.TestRaizLoad
import org.friendularity.vwmsg.VWSetupRq_Conf
import org.osgi.framework.BundleContext

import java.util.{Map => JMap, ArrayList => JArrayList}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}

trait EmulateVintageLaunch extends VarargsLogging {
	// Keys from   org.cogchar.bundle.app.vworld.central.VWorldMapperLifecycle
	val DEPKEY_Mediator = "puma-mediator";
	val DEPKEY_BodyHandleRec = "body-handle-rec";
	val DEPKEY_PumaRegCli = "puma-reg-client";
	val DEPKEY_CommandEvent = "command-event";
	val DEPKEY_AppContext = "app-context";

	// call this or similar from a lifecycle callback.  That's where we got this code!  Only issue is key management.
	// From   org.cogchar.bundle.app.vworld.central.VWorldMapperLifecycle
	def cookDish_fromDepMap(dependencyMap: JMap[String, AnyRef]): VWorldRegistry = {

		val pcMediator: PumaContextMediator = dependencyMap.get(DEPKEY_Mediator).asInstanceOf[PumaContextMediator]
		val ce: CommandEvent = dependencyMap.get(DEPKEY_CommandEvent).asInstanceOf[CommandEvent]
		val pumaRegCli: PumaRegistryClient = dependencyMap.get(DEPKEY_PumaRegCli).asInstanceOf[PumaRegistryClient]
		val bodyHandleRecList: JArrayList[BodyHandleRecord] = dependencyMap.get(DEPKEY_BodyHandleRec).asInstanceOf[JArrayList[BodyHandleRecord]]

		val pactx: PumaSysCtx = dependencyMap.get(DEPKEY_AppContext).asInstanceOf[PumaSysCtx]

		val repoCli : RepoClient = pactx.getSysCnfMgr().getMainConfigRC();
		val vbi = new VintageBodyIngredImpl(pcMediator, ce, pumaRegCli, bodyHandleRecList)

		cookDish(vbi, repoCli)
		// val helper: VWorldInitHelper = new VWorldInitHelper
		// helper.connectRegistry(vworldreg, pcMediator, pactx, ce, pumaRegCli, bodyHandleRecList)
		// From org.cogchar.bundle.app.vworld.central.VWorldInitHelper:
	}
	def cookDish(vbi : VintageBodyIngred, repoCli : RepoClient) : VWorldRegistry = {
		val doLegacyLaunch = false
		val doLegacyCinemaInit = false

		val pumaRegCli: PumaRegistryClient = vbi.getPumaRegCli // Used immediately to setup the vwreg
		val cvwreg = new CookVWReg
		cvwreg.setRegClient(pumaRegCli)

		if(doLegacyLaunch) {
			val pcMediator: PumaContextMediator = vbi.getPumaCtxMediator // Used in legacy-launch and for CTX-URI
			cvwreg.safelyLaunchLegacyVWorld(pcMediator)
		}

		val ccb = cvwreg.setupCmdBox(vbi, repoCli)

		val bodyHandleRecList: JArrayList[BodyHandleRecord] = vbi.getBodyHandleRecList
		ccb.connectBodies(bodyHandleRecList)

		if (doLegacyCinemaInit) {
			cvwreg.exposedInitCinema
		}
		cvwreg
	}
	// algebra term, incomplete semi-upgrade still uses repoCli, not very useful but instructive.
	def cookDish_FromScratch_usingOldRepoCli_Why(repoCli : RepoClient) : VWorldRegistry = {
		val bunCtx = null
		val mediator = null
		val regClient = new PumaRegistryClientImpl(bunCtx, mediator)
		// val vbi = new VintageBodyIngredImpl(mediator, ce, regClient, bodyHandleRecList)
		null
	}
	def cookDish_FromScratch(repoCli : RepoClient) : VWorldRegistry = {
		null
	}
}
class CookVWReg() extends StatefulVWorldRegistry () {
	override protected def initCinema(b: Boolean, classLoader: ClassLoader): Unit = ???

	def exposedInitCinema() = initCinema(false, null)

	override def getRouter: WantsThingAction = ???

	def setupCmdBox(vbi : VintageBodyIngred, repoClient: RepoClient) : CookCmdBox = {
		val pcMediator: PumaContextMediator = vbi.getPumaCtxMediator // Used in legacy-launch and for CTX-URI
		val ctxURI: String = pcMediator.getSysContextRootURI
		val ctxID: Ident = new FreeIdent(ctxURI)
		// val pactx: PumaSysCtx = vbi.getPumaAppCtx
		val ce: CommandEvent = vbi.getCmdEvt
		val ccb = setupCmdBox(ctxID, ce, repoClient)
		ccb
	}
	def setupCmdBox(ctxID : Ident, // pactx : PumaSysCtx,
					ce : CommandEvent, repoClient: RepoClient) : CookCmdBox = {
		val prc = getRegClient
		val boxSpc: BoxSpace  = prc.getTargetBoxSpace(null)
		val cmdSpc: CommandSpace = prc.getCommandSpace(null)
		val ccb = new CookCmdBox(this, boxSpc, ctxID) // , pactx)
		ce.setUpdater(ccb.asInstanceOf[Updater])
		setContextCommandBox(ccb)

		//		ccb.reloadCommandSpace
		TriggerItems.populateCommandSpace(repoClient, cmdSpc, boxSpc)
		ccb
	}
	// In newer launchers this is done elsewere
	@Deprecated def safelyLaunchLegacyVWorld(pcMediator: PumaContextMediator) : Boolean = {
		try {
			initVWorldUnsafe(pcMediator)
			true
		}
		catch {
			case t: Throwable => {
				getLogger.warn("%%%%%%%%%%%%%%%%%%%%%%% Error with VWorldMapper init %%%%%%%%%%%%%%%%%%%%%%%")
			}
				false
		}
	}
}
class CookCmdBox(myVWReg : VWorldRegistry, boxSpc: BoxSpace, ctxID : Ident) // , pactxWhy : PumaSysCtx)
			extends VWCtxCmdBox(myVWReg, boxSpc, ctxID) {

	// setAppContext(pactxWhy) // Old PumaAppCtx was used to find repoCli for command-trig load, and during system re-config.
	def connectBodies (bodyHandleRecList: JArrayList[BodyHandleRecord]) : Unit = {

		import scala.collection.JavaConversions._
		for (body <- bodyHandleRecList) {
			try {
				val boneSrcGraphID: Ident = body.getBoneSrcGraphID
				val repoCli: RepoClient = body.getRepoClient
				if (boneSrcGraphID != null) {
					getLogger.debug("boneSrcGraphID is non-null {}", boneSrcGraphID)
				}
				if (repoCli != null) {
					getLogger.debug("REPOCLIENT FOUND: {}", repoCli)
				}
				val humaFigCfg  = body.getHumaFigureConfig
				val figureID: Ident = humaFigCfg.getFigureID

				// This part can be done in any Java OpenGL env - OSGi is not needed.
				getLogger.info("Calling initVworldHumanoid for charID={} and boneSrcGraphID={}", figureID, boneSrcGraphID : Any)
				myVWReg.initVWorldHumanoid(body.getRepoClient, boneSrcGraphID, humaFigCfg)

				// This part requires connection to MechIO, which currently requires OSGi.
				getLogger.info("Calling connnectBonyRobotToHumanoidFigure for charID={}", figureID)
				myVWReg.connectBonyRobotToHumanoidFigure(body.getModelRobot, figureID)
			}
			catch {
				case t: Throwable => {
					getLogger.error("InitVWorldHumanoid failure")
				}
			}
		}
	}

	override protected def processUpdateRequestNow (request: String, resetMainConfigFlag: Boolean) : Boolean  = ???
}
trait EmuVintageVWCCB extends Updater {
	private var myRegClient: PumaRegistryClient = null
	// private var myRegClientOSGiComp: OSGiComponent[_] = null
	private var myBundleContext: BundleContext = null
	private var myBodyMgr: PumaDualBodyManager = null
	private var myBehavMgr: PumaBehaviorManager = null
	private var myPCCB: PumaContextCommandBox = null
	private var myBodyHandleRecs: JArrayList[BodyHandleRecord] = null

}
// Supplies pointers sufficient to launch bodies in VWorld in 2012-2014 style.
trait VintageBodyIngred {

	def getPumaRegCli: PumaRegistryClient

	// Legacy pre-lifecycle config injection point, still "works" but mostly unused.
	def getPumaCtxMediator: PumaContextMediator

	// def getPumaAppCtx: PumaSysCtx

	// Legacy high level app-system msg, forwards to Updater in same pkg,
	// one of         START_ANIMATION,	STOP_ANIMATION,	DATABALL_GOODY,	UPDATE;
	def getCmdEvt: CommandEvent

	def getBodyHandleRecList: JArrayList[BodyHandleRecord]
}
// Concrete record with the same pointers, init this from lifecycle event map or similar.
// pactx: PumaSysCtx,
case class VintageBodyIngredImpl(pcMediator: PumaContextMediator,
								 ce: CommandEvent, pumaRegCli: PumaRegistryClient,
								 bodyHandleRecList: JArrayList[BodyHandleRecord]) extends VintageBodyIngred {

	override def getPumaRegCli: PumaRegistryClient = pumaRegCli

	// Legacy pre-lifecycle config injection point, still "works" but mostly unused.
	override def getPumaCtxMediator: PumaContextMediator = pcMediator

	// override def getPumaAppCtx: PumaSysCtx = pactx

	// Legacy high level app-system msg, forwards to Updater in same pkg,
	// one of         START_ANIMATION,	STOP_ANIMATION,	DATABALL_GOODY,	UPDATE;
	override def getCmdEvt: CommandEvent = ce

	override def getBodyHandleRecList: JArrayList[BodyHandleRecord] = bodyHandleRecList
}

// case class InterimBodyIngredImpl()
*/