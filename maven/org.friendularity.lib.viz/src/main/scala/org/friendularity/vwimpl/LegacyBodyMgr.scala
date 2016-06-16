package org.friendularity.vwimpl

import akka.actor.ActorSystem
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.{EnhancedLocalRepoClient, RepoClient}
import org.cogchar.api.humanoid.{HumanoidFigureConfig, FigureConfig}
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.blob.emit.RenderConfigEmitter
import org.friendularity.cpump.CPStrongTeller
import org.friendularity.navui.{NavUiAppSvc, ExoBodyUserLogic}
import org.friendularity.rbody.{HumaConfHelper, BodyConnImpl}
import org.friendularity.vwmsg.{VWBodyNotice, VWBodyMoveRq, VWBodyRq}
import org.osgi.framework.BundleContext

/**
  * Created by Owner on 6/16/2016.
  */
class LegacyBodyMgr extends VarargsLogging {
	// Yet STILL a semi-old way of producing body conf (from legacy-style repo), but no longer buried under the PUMA.
	// It is now better, when possible, to instead pull the body conf from recipes and our finer, newer chunks,
	// and also to do that asynchronously upon request, compliant with lifecycles of model-blending-ctx guys.
	// We keep both alternatives alive to help during debugging.
	def startUpgradedYetLegacyBodyConn(bundleCtx: BundleContext, akkaSys: ActorSystem,
									   legacyELRC: EnhancedLocalRepoClient, appSvc: NavUiAppSvc) {
		val dualBodyID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#char_sinbad_88")
		val hmdGraphID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#hmd_sheet_22")
		val bonyGraphID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#bony_sheet_sinbad")
		val fullHumaCfg: HumanoidFigureConfig =
		getLogger.info("Posting patient char create Rq for body={}", dualBodyID)
		// val mbrsc: ModelBlendingRobotServiceContext = bci.getMBRSvcCtx
		// val bodyNoticer: CPStrongTeller[VWBodyNotice] = appSvc.makeExoBodyUserTeller(akkaSys, "coolBodyUser", userLogic)
		// appSvc.postPatientCharCreateRq(dualBodyID, fullHumaCfg, mbrsc, bodyNoticer)

	}
	def loadFullHumaConfig_SemiLegacy(legacyELRC: EnhancedLocalRepoClient, bundleCtx: BundleContext, dualBodyID : Ident, hmdGraphID : Ident, bonyGraphID : Ident) : HumanoidFigureConfig = {
		val partialFigCfg: FigureConfig = new FigureConfig(legacyELRC, dualBodyID, hmdGraphID)
		val bci: BodyConnImpl = new BodyConnImpl(bundleCtx, dualBodyID)
		val legacyRC_hooboy: RepoClient = legacyELRC
		bci.connectBonyRobot_usingOldRC(bundleCtx, partialFigCfg, bonyGraphID, legacyRC_hooboy)

		val hch: HumaConfHelper = new HumaConfHelper
		val noURI: Option[String] = scala.Option.apply(null)
		val rce: RenderConfigEmitter = new RenderConfigEmitter(noURI)
		val matPath: String = rce.getMaterialPath
		val fullHumaCfg: HumanoidFigureConfig = hch.finishOldConfLoad(partialFigCfg, legacyRC_hooboy, bonyGraphID, matPath)
		fullHumaCfg
	}
}
