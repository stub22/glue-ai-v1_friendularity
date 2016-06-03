package org.friendularity.rbody

import java.io.InputStream

import org.appdapter.core.log.BasicDebugger
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.RepoClient
import org.cogchar.api.skeleton.config.BoneRobotConfig
import org.cogchar.app.puma.body.PumaBodyGateway
import org.cogchar.app.puma.config.BodyHandleRecord
import org.cogchar.app.puma.registry.ResourceFileCategory
import org.cogchar.bind.mio.robot.client.RobotVisemeClient
import org.cogchar.bind.mio.robot.model.ModelRobot
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.bundle.app.vworld.central.VWorldRoboPump
import org.cogchar.platform.util.ClassLoaderUtils
import org.cogchar.render.app.humanoid.HumanoidRenderContext
import org.cogchar.render.model.humanoid.HumanoidFigure
import org.osgi.framework.BundleContext
import org.mechio.api.motion.Robot
import org.cogchar.api.humanoid.FigureConfig
import org.cogchar.api.skeleton.config.BoneRobotConfig
import org.cogchar.name.skeleton.BoneCN

import java.util.{List => JList, ArrayList => JArrayList}

trait BodyLogic extends VarargsLogging {
	// Stateless methods
	def setupRoboPump(pumpID: Ident, mr: ModelRobot, hf: HumanoidFigure): VWorldRoboPump = {
		val pump: VWorldRoboPump = new VWorldRoboPump(pumpID, mr, hf)
		pump.completeSetup
		return pump
	}
	protected def startVisemePump(bonyRobot : ModelRobot, bunCtx: BundleContext, clsForRKConf: JList[ClassLoader]) : Unit = {
		val robotId: Robot.Id = bonyRobot.getRobotId
		val robotVisCli: RobotVisemeClient = new RobotVisemeClient
		robotVisCli.startPumpingZenoAvatarVisemes(bunCtx, clsForRKConf, robotId)
	}

}

trait BodyConn extends BodyLogic {
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

	protected def getMBRSvcCtx : ModelBlendingRobotServiceContext = myMBRSvcCtx

	protected def getBundleCtx : BundleContext

}


/**
  * Code adapted from Cogchar PumaBodyGateway and PumaDualBody
  *
  * Next:  Reconcile with   VWorldRoboPum and VWorldRegistry -- HumanoidFigure
  */



class BodyConnImpl(myBunCtx : BundleContext) extends BodyConn  {
	private var myDualBodyID: Ident = null
	private var myNickName: String = null
	// private var bodyConfig: BodyHandleRecord = null

	override protected def getBundleCtx : BundleContext = myBunCtx

	def yuh(bundleCtx: BundleContext, humCfg : FigureConfig, bonyGraphID : Ident, rc : RepoClient) : Unit = {
		// PumaRegistryClient prc


		val mioConfCLs: JList[ClassLoader] = new JArrayList[ClassLoader]()
/*
		prc.getResFileCLsForCat(ResourceFileCategory.RESFILE_MIO_CONF)
		val extraCLs : util.List[ClassLoader] = myMediator.getExtraResFileCLsForCat(cat)
		val totalCLs : util.List[ClassLoader] = new util.ArrayList[ClassLoader](extraCLs)
		return totalCLs
 */
		val boneCN = new BoneCN
		val charID = myDualBodyID

		connectBonyRobotToMioAndVWorld(bundleCtx, humCfg, bonyGraphID, rc, boneCN, mioConfCLs)
		/*
		if (bodyConfig != null) {
			val bonyRobot : ModelRobot = myMBRSC.getRobot
			bodyConfig.setModelRobot(bonyRobot);
		}
		*/
	}

	def connectBonyRobotToMioAndVWorld(bundleCtx: BundleContext, hc: FigureConfig, qGraph: Ident,
									   qi: RepoClient, bqn: BoneCN, clsForRKConf: JList[ClassLoader]) : Boolean = {
		val boneRobotConf: BoneRobotConfig = new BoneRobotConfig(qi, myDualBodyID, qGraph, bqn)
		// Use this handle to call  .unregister() later, as needed.
		val brcServiceReg = bundleCtx.registerService(classOf[BoneRobotConfig].getName, boneRobotConf, null)
		val mbsrc = getMBRSvcCtx
		mbsrc.makeModelRobotWithBlenderAndFrameSource(boneRobotConf)
		val bonyRobot : ModelRobot = mbsrc.getRobot
		startVisemePump(bonyRobot, bundleCtx, clsForRKConf)
		startJointGroup(mbsrc, hc, clsForRKConf)
		true
	}


	protected def startJointGroup(mbrsc : ModelBlendingRobotServiceContext, hc: FigureConfig, possibleCLs: JList[ClassLoader]) : Unit = {
		val jgFullPath: String = hc.getJointGroupConfigPath
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
				// vworldreg.initVWorldHumanoid(body.getRepoClient, boneSrcGraphID, humaFigCfg)
				// which just does...
				// val hrc: HumanoidRenderContext = getVWM.getHumanoidRenderContext
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


}
/*
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
 */