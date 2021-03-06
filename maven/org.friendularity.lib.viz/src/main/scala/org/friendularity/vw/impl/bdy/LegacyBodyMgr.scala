package org.friendularity.vw.impl.bdy

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.{EnhancedLocalRepoClient, RepoClient}
import org.cogchar.api.humanoid.{FigureConfig, HumanoidFigureConfig}
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.blob.emit.RenderConfigEmitter
import org.friendularity.rbody.{BodyConnImpl, HumaConfHelper}
import org.osgi.framework.BundleContext

/**
  * Created by Stub22 on 6/16/2016.
  */
class LegacyBodyLoader_Stateless extends VarargsLogging {
	// Yet STILL a semi-old way of producing body conf (from legacy-style repo), but no longer buried under the PUMA.
	// It is now better, when possible, to instead pull the body conf from recipes and our finer, newer chunks,
	// and also to do that asynchronously upon request, compliant with lifecycles of model-blending-ctx guys.
	// We keep both alternatives alive to help during debugging.

	def loadFullHumaConfig_SemiLegacy(legacyELRC: EnhancedLocalRepoClient,
				dualBodyID : Ident, hmdGraphID : Ident, bonyGraphID : Ident) : HumanoidFigureConfig = {

		val partialFigCfg: FigureConfig = new FigureConfig(legacyELRC, dualBodyID, hmdGraphID)

		val hch: HumaConfHelper = new HumaConfHelper
		val noURI: Option[String] = scala.Option.apply(null)
		val rce: RenderConfigEmitter = new RenderConfigEmitter(noURI)
		val matPath: String = rce.getMaterialPath
		val fullHumaCfg: HumanoidFigureConfig = hch.finishOldConfLoad(partialFigCfg, legacyELRC, bonyGraphID, matPath)
		fullHumaCfg
	}
	// OK to pass in either a partial FigureConfig or a full HumanoidFigureConfig
	def connectMechIOBody(legacyELRC: EnhancedLocalRepoClient, bundleCtx: BundleContext,
						  partialFigCfg: FigureConfig, bonyGraphID : Ident) : ModelBlendingRobotServiceContext = {

		val dualBodyID = partialFigCfg.getFigureID
		val bci: BodyConnImpl = new BodyConnImpl(bundleCtx, dualBodyID)
		val legacyRC_hooboy: RepoClient = legacyELRC
		info3("Calling BodyConnImpl[bunCtx={},bodyID={}].connectBonyRobot_usingOldRC(bGrphID={}, legacyRC=yikes) - \n" +
					"to make a new ModelBlendingRobotServiceContext", bundleCtx, dualBodyID, bonyGraphID)
		bci.connectBonyRobot_usingOldRC(bundleCtx, partialFigCfg, bonyGraphID, legacyRC_hooboy)
		val mbrsc: ModelBlendingRobotServiceContext = bci.getMBRSvcCtx
		mbrsc
	}
}
