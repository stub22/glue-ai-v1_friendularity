package org.friendularity.rbody

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.skeleton.config.{BoneRotationAxis, BoneProjectionPosition}
import org.cogchar.app.puma.registry.{PumaRegistryClient, ResourceFileCategory}
import org.cogchar.bind.mio.robot.client.DirectRobotAnimContext
import org.cogchar.bind.mio.robot.model.{ModelRobot, ModelRobotUtils}
import org.cogchar.bind.mio.robot.svc.{ModelBlendingRobotServiceContext, RobotServiceContext}
import org.cogchar.blob.emit.BehaviorConfigEmitter
import org.cogchar.render.model.bony.{BoneState, FigureState}
import org.cogchar.render.model.humanoid.HumanoidFigure

import java.util.{Map => JMap, List => JList}
/**
  * Created by Stub22 on 9/6/2016.
  * Port of Cogchar code from ModelToFigureStateMappingFuncs.java and VWorldRoboPump.java.
  * This code is the primary intersection of o.c.mio(= mechio robot) with VWorld bones,
  * and thus has dependency implications as well as functional importance.
  */
trait RoboBoneStateMapping extends  VarargsLogging {
	def propagateState(br: ModelRobot, hf: HumanoidFigure) {
		val fs: FigureState = hf.getFigureState
		trace1("FigureState={}", fs)
		val rotMap: JMap[String, JList[BoneProjectionPosition]] = ModelRobotUtils.getGoalAnglesAsRotations(br)
		trace1("rotMap={}", rotMap)
		applyAllSillyEulerRotations(fs, rotMap)
	}

	def applyAllSillyEulerRotations(fs: FigureState, rotMap: JMap[String, JList[BoneProjectionPosition]]) {
		import scala.collection.JavaConversions._

		for (e <- rotMap.entrySet) {
			val me : JMap.Entry[String, JList[BoneProjectionPosition]] = e
			val boneName: String = me.getKey
			val bs: BoneState = fs.getBoneState(boneName)
			if (bs != null) {
				val rots: JList[BoneProjectionPosition] = me.getValue
				applySillyEulerRotations(bs, rots)
			}
			else {
				warn1("Can't find boneState for {}", boneName)

			}
		}
	}

	// This is not yet a viable technique, as rotations are not commutative!
	// Also, JME3 has some confusing direction labeling things going on - appears
	// that PITCH, ROLL, YAW are not defined in the traditional manner rel. to X, Y, Z.
	// Needs review!
	private def applySillyEulerRotations(bs: BoneState, rots: JList[BoneProjectionPosition]) {
		import scala.collection.JavaConversions._
		for (rot <- rots) {
			val rotAxis: BoneRotationAxis = rot.getRotationAxis
			val rads: Float = rot.getAngleRadians.asInstanceOf[Float]
			rotAxis match {
				case BoneRotationAxis.X_ROT =>
					bs.rot_X_A3rd = rads
				case BoneRotationAxis.Y_ROT =>
					bs.rot_Y_A1st = rads
				case BoneRotationAxis.Z_ROT =>
					bs.rot_Z_A2nd = rads
			}
		}
	}
}
class VWRoboBoneStatePump extends VarargsLogging {

}
/*
prc : PumaRegistryClient,
val clsForRKConf : JList[ClassLoader]  = prc.getResFileCLsForCat(ResourceFileCategory.RESFILE_MIO_CONF);

 */

// TODO:  We are currently missing some anim agent launch stuff.

		// In 2014 setup we had:
		/*
	PumaBooter.pumaBootUnsafeUnderOSGi
		final PumaSysCtx.BootSupport pscbs = new PumaSysCtxImplBootable(bundleCtx, mediator, ctxID);
		...
		pscbs.connectAllBodies();
			@Override public void connectAllBodies() {
				List<Ident> charIDs = getAllCharIdents();
				connectDualBodies(charIDs);					-- covered in 2016 code
				makeAgentsForAllBodies(charIDs);			-- NOT yet covered
					PumaDualBody pdb = myBodyMgr.getBody(charID);
					...
					myBehavMgr.makeAgentForBody(bunCtx, myRegClient, pdb, charID);

	PumaBehaviorManager
	public void makeAgentForBody(BundleContext bunCtx, PumaRegistryClient pRegCli, PumaDualBody pdb, Ident agentID) {

		DirectBehaviorAgent pbAgent = new DirectBehaviorAgent(agentID, myBehavCE);
		RobotServiceContext  optLocalRobotSvcCtx = null;
		PumaBodyGateway pBodGate = pdb.getBodyGateway();
		if (pBodGate != null) {
			optLocalRobotSvcCtx = pBodGate.getRobotServiceContext();
		}
		pbAgent.initMappers(pRegCli, optLocalRobotSvcCtx);
		String chanGraphQN =  "ccrt:chan_sheet_AZR50",  behavGraphQN  =  "hrk:behav_file_44";
		// See stack trace below for a good clue as to what this method accomplishes:
		pbAgent.setupAndStart(bunCtx, pRegCli, chanGraphQN, behavGraphQN);

		myBehaviorAgentList.add(pbAgent);
	}

public class DirectBehaviorAgent extends PumaBehaviorAgent {
	public void initMappers(PumaRegistryClient prc, RobotServiceContext rsc) {
		List<ClassLoader> clsForRKConf = prc.getResFileCLsForCat(ResourceFileCategory.RESFILE_MIO_CONF);
		myRobotMotionMapper = new PumaRobotMotionMapper(myAgentID, myBehaviorCE, clsForRKConf, rsc);
		mySpeechOutputMapper = new PumaSpeechOutputMapper(myAgentID);
	}
	public void connectAnimOutChans(Theater thtr) {
		FancyTextPerfChan bestAnimOutChan = myRobotMotionMapper.getBestAnimOutChan();
------------
	protected FancyTextPerfChan getBestAnimOutChan() {
		AnimOutTrigChan aotc = myRobotAnimCtx.getTriggeringChannel();



public class PumaRobotMotionMapper extends BasicDebugger {
	private	Ident				myAnimOutTrigChanID;
	private	RobotServiceContext	myRobotSvcCtx;
	private	RobotAnimContext	myRobotAnimCtx;

	public PumaRobotMotionMapper (Ident animOutTrigChanID, BehaviorConfigEmitter behavCE, List<ClassLoader> clsForRKConf,
				RobotServiceContext optLocalRobotSvcContext)  {
		myAnimOutTrigChanID = animOutTrigChanID;
		myRobotSvcCtx = optLocalRobotSvcContext;
		if (behavCE == null) {
			getLogger().warn("Cannot init with behavCE == null");
			return;
		}
		// Set up our animation triggering context, connecting behavior system to scripted-animation system.
		if (optLocalRobotSvcContext != null) {
				// This way is used for direct connect to a local robot graph, bypassing some abstractions.
            try{
                DirectRobotAnimContext drac = new DirectRobotAnimContext(animOutTrigChanID, behavCE, optLocalRobotSvcContext);
                myRobotAnimCtx = drac;
            }catch(RuntimeException ex){
                getLogger().warn("Error creating DirectRobotAnimContext.", ex);
                myRobotAnimCtx = new RobotAnimContext(myAnimOutTrigChanID, behavCE);
            }
		} else {
			// This way is most general case for channel + lifecycle wiring.
			myRobotAnimCtx = new RobotAnimContext(myAnimOutTrigChanID, behavCE);
		}
		if (clsForRKConf != null) {
			// Setup classLoaders used to load animations
			myRobotAnimCtx.setResourceClassLoaders(clsForRKConf);
		}
	}

*****************/

/****************
  *
  *  If  BehaviorConfigEmitter's RepoClient is null in init() above, we will later get:
 *  [java] 	at org.cogchar.blob.emit.BehaviorConfigEmitter.getAnimPathResolverModel(BehaviorConfigEmitter.scala:33)
 [java] 	at org.cogchar.bind.rk.robot.client.RobotAnimContext.makeMediaHandleCache(RobotAnimContext.java:148)
 [java] 	at org.cogchar.bind.rk.robot.client.RobotAnimContext.getTriggeringChannel(RobotAnimContext.java:141)
 [java] 	at org.cogchar.app.puma.behavior.PumaRobotMotionMapper.getBestAnimOutChan(PumaRobotMotionMapper.java:76)
 [java] 	at org.cogchar.app.puma.behavior.DirectBehaviorAgent.connectAnimOutChans(DirectBehaviorAgent.java:56)
 [java] 	at org.cogchar.app.puma.behavior.DirectBehaviorAgent.doWiringPostStart(DirectBehaviorAgent.java:75)
 [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorAgent.startTheater(PumaBehaviorAgent.java:91)
 [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorAgent.setupAndStartBehaviorTheater(PumaBehaviorAgent.java:81)
 [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorAgent.setupAndStart(PumaBehaviorAgent.java:60)
 [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorManager.makeAgentForBody(PumaBehaviorManager.java:72)
 [java] 	at org.cogchar.app.puma.boot.PumaSysCtxImpl.makeAgentsForAllBodies(PumaSysCtxImpl.java:180)
 [java] 	at org.cogchar.app.puma.boot.PumaSysCtxImpl.connectAllBodies(PumaSysCtxImpl.java:168)
 [java] 	at org.cogchar.app.puma.boot.PumaBooter.pumaBootUnsafeUnderOSGi(PumaBooter.java:144)
 [java] 	at org.cogchar.app.puma.boot.PumaBooter.bootUnderOSGi(PumaBooter.java:77)
 [java] 	at com.hrkind.bundle.opengl.R50.ActivatorPumaOpenglR50.startPumaDemo(ActivatorPumaOpenglR50.java:103)
 [java] 	at com.hrkind.bundle.opengl.R50.ActivatorPumaOpenglR50.handleFrameworkStartedEvent(ActivatorPumaOpenglR50.java:66)

	 */

/*
	@Override public List<ClassLoader> getResFileCLsForCat(ResourceFileCategory cat) {
		List<ClassLoader> extraCLs = myMediator.getExtraResFileCLsForCat(cat);
		List<ClassLoader> totalCLs = new ArrayList<ClassLoader>(extraCLs);
		getLogger().warn("As of 2014-04-26, we are no longer supplying the ResourceBundle ClassLoader here!");
		// ClassLoader ourOpenGLResLoader = org.cogchar.bundle.render.resources.ResourceBundleActivator.class.getClassLoader();
		// totalCLs.add(ourOpenGLResLoader);
		return totalCLs;
	}


2014 RK code does this in mediator
	public List<ClassLoader> getExtraResFileCLsForCat(
            ResourceFileCategory cat) {

		List<ClassLoader> extraCLs = new ArrayList<ClassLoader>();
		extraCLs.add(PreviewContentBundleActivator.class.getClassLoader());
		return extraCLs;
	}
 */


/*
PumaBehaviorManager
	public void initConfigLinks(PumaRegistryClient prc) {
		PumaContextMediator mediator = prc.getCtxMediator(null);
		// These values trickle through:
		// BehaviorConfigEmitter, DirectBehaviorAgent, PumaRobotMotionMapper, DirectRobotAnimContext,
		// and finally into the AnimMediaHandle.Cache  which is used by the AnimOutTrigChan.

		RepoClient animResRepoClient = prc.getConfigMgr(null).getMainConfigRepoClient();
		Ident animPathModelID =  animResRepoClient.getDefaultRdfNodeTranslator().makeIdentForQName(AnimFileSpecReader.animGraphQN());
		myBehavCE = new BehaviorConfigEmitter(animResRepoClient, animPathModelID);

		// Here are some older config properties serving a related config/anim path-resolving role.
		String sysContextURI = mediator.getSysContextRootURI();
		if (sysContextURI != null) {
			myBehavCE.setSystemContextURI(sysContextURI);
		}
		String filesysRootPath = mediator.getOptionalFilesysRoot();
		if (filesysRootPath != null) {
			myBehavCE.setLocalFileRootDir(filesysRootPath);
		}


BehaviorCE eventually gets used in RobotAnimContext:

	private AnimMediaHandle.Cache makeMediaHandleCache() {
		// This behavCE was set up by either:    PumaBehaviorManager   or    CCRK_ServiceChannelFactory
		BehaviorConfigEmitter behavCE = myBehaviorCE;
		// If there is not enough info in the behavCE, the returned list will be empty
		scala.collection.immutable.List animFancyFileSpecs = AnimFileSpecReader.findAnimFileSpecs(behavCE);
		MediaPathFinder pathFinder = MediaResolverFactory.makeFancyFileSpecMediaPathFinder(animFancyFileSpecs);
		UrlSearcher searcher = MediaResolverFactory.makeClasspathUrlSearcher(myResourceCLs);
		return new AnimMediaHandle.Cache(myAnimClient, pathFinder, searcher); //  pathModel, pathPropID, myResourceCLs);
	}

... but that is only called if we use
getTriggeringChannel

 */