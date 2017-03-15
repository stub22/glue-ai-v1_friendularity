/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.rbody

import java.io.InputStream
import java.util.{List => JList, ArrayList => JArrayList}
import java.util.concurrent.{Callable, Future}

import com.jme3.math.Vector3f
import com.jme3.scene.Spatial
import org.appdapter.core.log.BasicDebugger
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.RepoClient
import org.cogchar.api.skeleton.config.BoneRobotConfig
import org.friendularity.vw.impl.manip.Manipable
import org.friendularity.vwobstub.VWorldRoboPump

//import org.cogchar.app.puma.body.PumaBodyGateway
//import org.cogchar.app.puma.config.BodyHandleRecord
//import org.cogchar.app.puma.registry.ResourceFileCategory
import org.cogchar.bind.mio.robot.client.{RobotAnimClient, DirectRobotAnimContext, RobotAnimContext, RobotVisemeClient}
import org.cogchar.bind.mio.robot.model.ModelRobot
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.blob.emit.{BehaviorConfigEmitter, RenderConfigEmitter}


import org.cogchar.platform.util.ClassLoaderUtils
import org.cogchar.render.app.humanoid.HumanoidRenderContext
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.cogchar.render.model.humanoid.{VWorldHumanoidFigureEntity, HumanoidFigureManager, HumanoidFigure}
import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.cogchar.render.sys.goody.GoodyRenderRegistryClient
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.CogcharRenderSchedule
import org.cogchar.render.sys.task.Queuer.QueueingStyle

import org.osgi.framework.BundleContext
import org.mechio.api.motion.Robot
import org.cogchar.api.humanoid.{HumanoidFigureConfig, FigureConfig}
import org.cogchar.api.skeleton.config.BoneRobotConfig
import org.cogchar.name.skeleton.BoneCN

import java.util.{List => JList, ArrayList => JArrayList}

trait BonyRobotInitFuncs extends VarargsLogging {
	protected def startVisemePump(bonyRobot : ModelRobot, bunCtx: BundleContext, clsForRKConf: JList[ClassLoader]) : Unit = {
		val robotId: Robot.Id = bonyRobot.getRobotId
		val robotVisCli: RobotVisemeClient = new RobotVisemeClient
		robotVisCli.startPumpingZenoAvatarVisemes(bunCtx, clsForRKConf, robotId)
	}
	protected def startJointGroup(mbrsc : ModelBlendingRobotServiceContext, partialFigConf: FigureConfig, possibleCLs: JList[ClassLoader]) : Unit = {
		val jgFullPath: String = partialFigConf.getJointGroupConfigPath
		if (jgFullPath != null) {
			val cl: ClassLoader = ClassLoaderUtils.findResourceClassLoader(jgFullPath, possibleCLs)
			if (cl != null) {
				val stream: InputStream = cl.getResourceAsStream(jgFullPath)
				if (stream != null) {
					mbrsc.startJointGroup(stream)
				}
			}
		}
	}
	def connectBonyRobotToMio(bundleCtx: BundleContext, mbsrc : ModelBlendingRobotServiceContext,
							  partialFigConf: FigureConfig, boneRobotConf: BoneRobotConfig,
							  clsForRKConf: JList[ClassLoader]) : Boolean = {

		// Use this handle to call  .unregister() later, as needed.
		info1("connectBonyRobotToMio called for conf={}", boneRobotConf)
		val brcServiceReg = bundleCtx.registerService(classOf[BoneRobotConfig].getName, boneRobotConf, null)
		mbsrc.makeModelRobotWithBlenderAndFrameSource(boneRobotConf)
		val bonyRobot : ModelRobot = mbsrc.getRobot
	//	startVisemePump(bonyRobot, bundleCtx, clsForRKConf)
	//	startJointGroup(mbsrc, partialFigConf, clsForRKConf)
		true
	}
}
trait HumaFigureInitFuncs extends VarargsLogging {
	def makeHumaFigCfg(partialFigCfg : FigureConfig, repoCli : RepoClient,
					   bonyGraphID : Ident, matPath : String) : HumanoidFigureConfig = {
		val hfc = new HumanoidFigureConfig(repoCli, partialFigCfg, matPath, bonyGraphID);
		hfc
	}
	def makeAndAttachHumaFigure_isBlocking(pmrc : PhysicalModularRenderContext, hfm : HumanoidFigureManager, humaFigCfg : HumanoidFigureConfig) : HumanoidFigure = {

		val hf : HumanoidFigure = hfm.addHumanoidFigure(humaFigCfg)
		hfm.attachFigure(pmrc, hf)  // Blocks until attachment complete on VW render thread
		hf
	}
}
trait DualBodyInitFuncs extends VarargsLogging {
	// Methods which appear to be stateless, although they may cause some side effects.
	private def setupRoboPump(dualBodyID: Ident, mr: ModelRobot, hf: HumanoidFigure): VWorldRoboPump = {
		val pump: VWorldRoboPump = new VWorldRoboPump(dualBodyID, mr, hf)
		pump.completeSetup
		return pump
	}

	def attachBonyRobotToFigure(mbsrc : ModelBlendingRobotServiceContext, figID : Ident, hf : HumanoidFigure): VWorldRoboPump = {
		val bonyRobot : ModelRobot = mbsrc.getRobot
		debug1("Calling connectBonyRobotToHumanoidFigure for charID={}", figID)

		val pump: VWorldRoboPump = setupRoboPump(figID, bonyRobot, hf)
		pump
	}
}
class DualBodyHelper() extends HumaFigureInitFuncs with DualBodyInitFuncs {
	// Cannot be called until we have both a happy V-World context and a happy Model-BoneRobot conn to MechIO.
	def finishDualBodInit(dualBodyID: Ident, mbsrc_opt : Option[ModelBlendingRobotServiceContext], pmrc: PhysicalModularRenderContext,
						  hfm : HumanoidFigureManager, hfConf  : HumanoidFigureConfig) : DualBodyRecord = {
		info1("******* ********************** Calling makeAndAttachHumaFigure for dualBodyID={}", dualBodyID)
		val humaFig : HumanoidFigure = makeAndAttachHumaFigure_isBlocking(pmrc, hfm, hfConf)


		val vwRoboPump_opt : Option[VWorldRoboPump] = mbsrc_opt.map(mrbsc => {
			info1("******* **********************  attachBonyRobotToFigure for dualBodyID={}", dualBodyID)
			attachBonyRobotToFigure(mrbsc, dualBodyID, humaFig)
		})

		info1("******* ********************** Finished attaching bony robot and human figure for dualBodyID={}", dualBodyID)
		val rrc =  pmrc.getRenderRegistryClient
		new DualBodyRecord(dualBodyID, hfConf, humaFig, vwRoboPump_opt, rrc, mbsrc_opt)
	}
}
trait JmeQueueClient {
	// See also "Queuer.java" in o.c.lib.render.impl/o.c.render.sys.task
	protected def getRenderTaskScheduler : CogcharRenderSchedule

	def enqueueTask[V](taskToCall: Callable[V]): Future[V] = {
		val scheduler = getRenderTaskScheduler
		scheduler.enqueue(taskToCall)
	}

}
class EnhRobotAnimContext(animOutChanID : Ident, behavCE : BehaviorConfigEmitter,
						  robotSvcContext : ModelBlendingRobotServiceContext)
			extends  DirectRobotAnimContext(animOutChanID, behavCE, robotSvcContext) {
	def getAnimClient : RobotAnimClient = myAnimClient
	def getRobot : ModelRobot = getModelRobot
}
trait SetupRobotAnimCtx {

	def connectDirectRoboAnimCtx(agentID : Ident, rsc : ModelBlendingRobotServiceContext,
								 clsForConf : JList[ClassLoader],
								 behavCE : BehaviorConfigEmitter ) : EnhRobotAnimContext = {
		val animOutTrigChanID : Ident = agentID
		val drac = new EnhRobotAnimContext(animOutTrigChanID, behavCE, rsc)
		// Setup classLoaders used to load animations
		drac.setResourceClassLoaders(clsForConf);
		drac
	}
}

case class DualBodyRecord(dualBodyID: Ident, hfConf  : HumanoidFigureConfig,
						  humaFig: HumanoidFigure, vwRoboPump_opt : Option[VWorldRoboPump],
						  rrc : RenderRegistryClient,  mbsrc_opt : Option[ModelBlendingRobotServiceContext] )
			extends JmeQueueClient with Manipable with SetupRobotAnimCtx {

	override def getRenderTaskScheduler : CogcharRenderSchedule = rrc.getWorkaroundAppStub

	// Called an orphan because it is not part of a 2012-2014 entity-space
	lazy val myOrphanEntity = new VWorldHumanoidFigureEntity(rrc, hfConf.getFigureID, humaFig)

	lazy val myRobotAnimCtx_opt : Option[RobotAnimContext] = mbsrc_opt.map(mbsrc => {
		val clsForConf : JList[ClassLoader] = new JArrayList[ClassLoader]()
		connectDirectRoboAnimCtx(dualBodyID, mbsrc, clsForConf, null)
	})
	def moveVWBody_usingEntity(x : Float, y : Float, z : Float) : Unit = {
		val posVec3f = new Vector3f(x, y, z)
		myOrphanEntity.setPosition(posVec3f, QueueingStyle.QUEUE_AND_RETURN)
	}
	def moveVWBody_usingNode(x : Float, y : Float, z : Float) : Unit = {
		val moveTask = new Callable[Unit]() {
			override def call(): Unit = {
				val vec = new Vector3f(x, y, z)
				// oops, need node or other methods that are protected in humaFig
			}
		}
	}

	def unused_because_vwbody_should_be_autonomous_registerAsHumaGoody(bgc : BasicGoodyCtx, grrc: GoodyRenderRegistryClient) : Unit = {
		val vhfe: VWorldHumanoidFigureEntity = new VWorldHumanoidFigureEntity(grrc,  hfConf.getFigureID, humaFig)
		bgc.getVWER.addGoody(vhfe)

	}
	// These two methods required to be Smoovable!
	override def getMainSpat: Spatial = humaFig.getNode // Could instead call getFigureNode, which returns same thing
	override def getID: Ident = dualBodyID
}


class HumaConfHelper() extends HumaFigureInitFuncs {
	def finishOldConfLoad(partialFigCfg : FigureConfig, repoCli : RepoClient, bonyGraphID : Ident, matPath : String) : HumanoidFigureConfig = {
		info2("******* ********************** Calling makeHumaFigCfg with bonyGraph={} and matPath={}", bonyGraphID, matPath)
		val hfConf: HumanoidFigureConfig = makeHumaFigCfg(partialFigCfg, repoCli, bonyGraphID, matPath)
		info1("Got completed huma-fig-conf: {}", hfConf)
		hfConf
	}
}
trait BodyConn extends VarargsLogging {
	// Connection info for a *single* character body.

	// These RobotServiceContext types are confusing, because the instance represents a single robot conn
	// (with a myRobot instance variable accessed via getRobot), but there is also a lot of static state
	// and methods that are used to keep track of "all robots" within the same class.
	//
	// ModelBlendingRobotServiceContext

	private lazy val myMBRSvcCtx = {
		val bunCtx = getBundleCtx
		new ModelBlendingRobotServiceContext(bunCtx)
	}

	def getMBRSvcCtx : ModelBlendingRobotServiceContext = myMBRSvcCtx

	protected def getBundleCtx : BundleContext

	protected def getDualBodyID : Ident

	def doTheBusiness : Unit = {

	}

}


/**
  * Code adapted from Cogchar PumaBodyGateway and PumaDualBody
  *
  * Next:  Reconcile with   VWorldRoboPum and VWorldRegistry -- HumanoidFigure
  */


class BodyConnImpl(myBunCtx : BundleContext, myDualBodyID : Ident) extends BodyConn	with BonyRobotInitFuncs {

	private var myNickName: String = null
	// private var bodyConfig: BodyHandleRecord = null

	override protected def getBundleCtx : BundleContext = myBunCtx

	override protected def getDualBodyID : Ident = myDualBodyID

	// The Graph and RepoClient are needed to allow completion of the BoneRobotConfig using the OLDE pathways.
	// This only connects to OSGi MechIO bony-model robot movement features.
	// It does not init any of the Avatar VWorld features.
	def connectBonyRobot_usingOldRC(bunCtx : BundleContext, partialFigCfg : FigureConfig, bonyGraphID : Ident,
									rc : RepoClient) : Unit = {
		// PumaRegistryClient prc

		val mioConfCLs: JList[ClassLoader] = new JArrayList[ClassLoader]()
/* 2014 classloader setup
		prc.getResFileCLsForCat(ResourceFileCategory.RESFILE_MIO_CONF)
		val extraCLs : util.List[ClassLoader] = myMediator.getExtraResFileCLsForCat(cat)
		val totalCLs : util.List[ClassLoader] = new util.ArrayList[ClassLoader](extraCLs)
		return totalCLs
 */
		val boneCN = new BoneCN

		val bodyID = getDualBodyID
		val boneRobotConf: BoneRobotConfig = new BoneRobotConfig(rc, bodyID, bonyGraphID, boneCN)

		val mbrsc = getMBRSvcCtx
		connectBonyRobotToMio(bunCtx, mbrsc, partialFigCfg, boneRobotConf, mioConfCLs)

	}

}
/*
Old ways we are replacing looked like this:

	graphIdentForBony = pgmm.resolveGraphForCharAndRole(charIdent, EntityRoleCN.BONY_CONFIG_ROLE);
	graphIdentForHumanoid = pgmm.resolveGraphForCharAndRole(charIdent, EntityRoleCN.HUMANOID_CONFIG_ROLE);

	  val humConfig : FigureConfig = new FigureConfig(rc, charIdent, graphIdentForHumanoid)
	  val pdb : PumaDualBody = connectDualBody(humConfig, graphIdentForBony)

            ServiceLifecycleProvider<ArrayList> lifecycle =
                new SimpleLifecycle<ArrayList>(myBodyHandleRecs,ArrayList.class.getName());
            Properties props=new Properties();
            props.put("bodyConfigSpec","bodyConfigSpec");
            ManagedService<ArrayList> ms = new OSGiComponent<ArrayList>(myBundleContext, lifecycle, props);
            ms.start();

            ClassLoader vizResCL = getSingleClassLoaderOrNull(ResourceFileCategory.RESFILE_OPENGL_JME3_OGRE);

	protected PumaDualBody connectDualBody(FigureConfig humCfg, Ident graphIdentForBony) throws Throwable {
		Ident bonyCharID = humCfg.getFigureID();
		BundleContext bunCtx = getBundleContext();
		RepoClient rc = getMainConfigRC();
        //bodyConfigSpecs.add(new BodyConfigSpec(rc, bonyCharID, humCfg));
		PumaDualBody pdb = new PumaDualBody(bonyCharID, humCfg.getNickname());
		// Create and publish a BodyHandleRecord so that other systems can discover this body.
		BodyHandleRecord bConfig = new BodyHandleRecord(rc, graphIdentForBony, humCfg);
        pdb.setBodyConfigSpec(bConfig);
        pdb.absorbContext(myRegClient, bunCtx, rc, humCfg, graphIdentForBony);
		myBodyHandleRecs.add(bConfig);
        myBodyMgr.addBody(pdb);
		return pdb;
	}

	def ok(hrc: HumanoidRenderContext, bodyHandleRecList : List[BodyHandleRecord]) : Unit = {
		import scala.collection.JavaConversions._
		for (body <- bodyHandleRecList) {
			try {
				val boneSrcGraphID: Ident = body.getBoneSrcGraphID
				val repoCli: RepoClient = body.getRepoClient
				if (boneSrcGraphID != null) {
					debug1("boneSrcGraphID is non-null {}", boneSrcGraphID)
				}
				if (repoCli != null) {
					debug1("REPOCLIENT FOUND: {}", repoCli)
				}
				val humaFigCfg: FigureConfig = body.getHumaFigureConfig
				val figureID: Ident = humaFigCfg.getFigureID
				info2("Calling initVworldHumanoid for charID={} and boneSrcGraphID={}", figureID, boneSrcGraphID)

				// The repoCli is needed because hfm.setupHumanoidFigure calls hfm.getOrMakeHumanoidFigure,
				// which makes an expanded HumanoidFigureConfig based on the supplied FigureConfig and the
				// contents of the config graph.
				val hf: HumanoidFigure = hrc.getHumanoidFigureManager.setupHumanoidFigure(hrc, repoCli, figureID, boneSrcGraphID, humaFigCfg)
				val mbsrc = getMBRSvcCtx
				val bonyRobot : ModelRobot = mbsrc.getRobot
				info1("Calling connnectBonyRobotToHumanoidFigure for charID={}", figureID)

				val pump: VWorldRoboPump = setupRoboPump(figureID, bonyRobot, hf)
			}
			catch {
				case t: Throwable => {
					getLogger.error("InitVWorldHumanoid failure")
				}
			}
		}
	}
*/