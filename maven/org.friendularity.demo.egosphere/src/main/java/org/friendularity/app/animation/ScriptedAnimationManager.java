/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.animation;

import org.friendularity.model.io.VSA_Reader;
import org.friendularity.nwrap.joint.JointAnimationPacket;
import org.friendularity.nwrap.joint.JointCommand;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.api.animoid.config.bonus.ServoChannelConfig;
import org.cogchar.api.animoid.protocol.Animation;
import org.cogchar.api.animoid.protocol.Device;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.Joint;
import org.cogchar.api.animoid.protocol.JointPositionAROM;
import org.cogchar.api.animoid.protocol.JointPositionCache;
import org.cogchar.api.animoid.protocol.Library;

/**
 *
 * @author Stu Baurmann
 */
public class ScriptedAnimationManager {
	private static Logger	theLogger = Logger.getLogger(ScriptedAnimationManager.class.getName());

	private		AnimoidFacade		myAnimoidFacade;

	public ScriptedAnimationManager(AnimoidFacade af) {
		myAnimoidFacade = af;
	}
	public  void loadAnimationLibrary(String animDirPath) throws Throwable {
		Library animLib = loadAnimationsFromDirectory(animDirPath);
		myAnimoidFacade.setAnimationLibrary(animLib);
	}
	public List<String> getAvailableAnimationNames() {
		return myAnimoidFacade.getAnimationLibrary().getAnimationNames();
	}
	private  Library loadAnimationsFromDirectory(String animDirPath) throws Throwable {
		theLogger.fine("BlendingAnimator - loading animations from: " + animDirPath );
		Library animLib = new Library();
		ServoChannelConfig[] servoConfs = myAnimoidFacade.getServoChannelConfigSparseArray();

		List<JointAnimationPacket> animList = new ArrayList<JointAnimationPacket>();
		if ((animDirPath == null) || (animDirPath.equals("NULL"))) {
			// No anims
			theLogger.warning("No animations loaded because animDirPath=" + animDirPath);
			animList = new ArrayList<JointAnimationPacket>();
		} else {
			JointPositionCache<JointPositionAROM> jpCache = new JointPositionCache<JointPositionAROM>();
//			Thru April 2010 we were building a list of JointAnimationPackets
//			(one for each animation file).
//			But this requires a very large amount of memory to be allocated.
//			(~500M for Bina's 170 animations)
//			It's better to read the packets and translate them one at a time.
//			animList = VSA_Reader.readAnimationsInDirectory(animDirPath, servoConfs, true);
//			for(JointAnimationPacket janip : animList) {
			File[]		files = VSA_Reader.getAnimationFilesInDirectory(animDirPath);
			for (int i=0; i < files.length; i++) {
				JointAnimationPacket janip = VSA_Reader.readAnimationFile(files[i], servoConfs, true);
				if (janip != null) {
					String animName = janip.animationName.get();
					OldAnimationScript animScript = new OldAnimationScript(janip);
					animScript.buildChannelScripts(servoConfs);

					Animation denseAbsAnim = buildAnimationAsDenseAbsROM(animScript, 0.1, jpCache);
					if (jpCache != null) {
						theLogger.info("unique scripted JointPositions=" + jpCache.myRegisteredCounter + ", cacheHits=" + jpCache.myCacheHitCounter);
					}
					// theLogger.info("Used joints in " + animName + " are: " + denseAbsAnim.getUsedJointSet());
					animLib.registerAnimation(denseAbsAnim);
				}
			}
		}
		return animLib;
	}
	public Animation buildAnimationAsDenseAbsROM(OldAnimationScript script, Double framePeriodSec,
				JointPositionCache<JointPositionAROM> jpCache) throws Throwable {
		Device device = myAnimoidFacade.getMainDevice();
		Animation animation = new Animation(script.getName(), framePeriodSec);
		int frameCount = script.getFrameCount();
		animation.appendEmptyFrames(frameCount);
		for (OldServoChannelScript scs : script.getChannelScripts()) {
			ServoChannelConfig scc = scs.getChannelConfig();
			int physicalChannelIDX = scc.physicalChannel;
			String deviceChannelID = "" + physicalChannelIDX;
			Joint j = device.getJointForChannelID(deviceChannelID);
			int firstNonemptyFrameIDX = scs.findNextNonemptyFrameIndex(0);
			if (firstNonemptyFrameIDX == -1) {
				continue;
			}
			double firstLopsidedPos = scs.getPositionValueAtIndex(firstNonemptyFrameIDX);
			double lastResolvedROM = scc.convertLopsidedFloatToROM(firstLopsidedPos);
			for (int frameIDX = 0; frameIDX < frameCount; frameIDX++) {
				OldServoChannelScript.Command cmd = scs.getCommandAtIndex(frameIDX);
				if (cmd.jointCommand == JointCommand.MOVE_ABSOLUTE_IMMEDIATE) {
					lastResolvedROM = scc.convertLopsidedFloatToROM(cmd.positionValue);
				}
				Frame f = animation.getFrameAt(frameIDX);
				JointPositionAROM jp = new JointPositionAROM(j, lastResolvedROM);
				if(jpCache != null) {
					jp = jpCache.findOrRegisterJointPos(jp);
				}
				f.addPosition(jp);
			}
		}
		return animation;
	}
	public synchronized void playAnimation(String animName, String gestureName,
				double rashAllowanceMultiplier, double rashBonusAllowance) {
		theLogger.info("Starting animation named: " + animName + ", for gesture " + gestureName);
		myAnimoidFacade.suggestAnimationScriptName(animName, gestureName, rashAllowanceMultiplier, rashBonusAllowance);
		// The output frame contains all the values WRITTEN on the last transform cycle.
		// It is mostly true that these are the only values we expect to be offset
		// from default, given a lot of current assumptions.  BUT, this is not a very
		// robust relationship, and it needs to be revisited as we further design the
		// java animator.  Alternatives
		// 1) Use the last input frame, which is one frame behind, but guaranteed comprehensive.
		// 2) Make sure output frame is always comprehensive after each transform (start by
		// copying inputFrame to output frame)
		// 3)

		// But, what about servo channels from an interrupted animation which are NOT written
		// by the new animation?!  Those will be left hanging, because we don't extend
		// the channel width of the new animation, right?
		/*
		JointFrameStruct lastOutputFrame = myJointFrameTransformer.getOutputFrame();
		execAnim.adjustForCurrentPositions(lastOutputFrame, transitionWindowFrameCount,
					transitionMergeThreshold);
		 */
	}
}
