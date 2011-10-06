/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.jmxwrap;

import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.person.PersonResolver;
import org.friendularity.gui.freckle.QueryImagePanel;
import org.cogchar.speech.ITTSEngineObserver;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

import org.friendularity.gui.vision.VisionMonitorChannelImpl;
import java.util.Observable;
import org.cogchar.ancient.utility.Parameters;
import org.cogchar.animoid.broker.AnimoidCueSpaceStub;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.convoid.cue.ConvoidCueSpace;
import org.cogchar.integroid.awareness.AwarenessHelpFuncs;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.integroid.broker.IntegroidHelpFuncs;
import org.cogchar.integroid.broker.ReactionProcessor;
import org.cogchar.integroid.cue.MotionCue;
import org.cogchar.platform.stub.CueSpaceStub;
import org.cogchar.platform.stub.JobSpaceStub;
import org.cogchar.platform.util.TimeUtils;
import org.cogchar.sight.motion.PeakTracker;
import org.cogchar.vision.MotionFilterObserver;
import org.cogchar.vision.PortableImage;
import org.freckler.extra.FreckbaseFacade;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;
import org.friendularity.webber.comm.Communicator;
import org.friendularity.webber.config.MeneConfig;

/**
 * @Deprecated Ad-hoc singleton for sending various signals to/from the Thalamus and JMX Clients
 * @author Stu Baurmann
 * 
 * // OldAnimationProgressListener
 */
public class SignalStation extends Observable implements  ITTSEngineObserver  {
    private static Logger	theLogger = Logger.getLogger(SignalStation.class.getName());
	
	private static SignalStation theSignalStation;
	
	private	IntegroidFacade		myIntegroidFacade;

	private ReactionProcessor	myReactionProcessor;

	public QueryImagePanel		myDetectedImagePanel;

	public	PersonResolver		myPersonResolver;

	private		FreckbaseFacade		myFreckbaseFacade;

	private	long				myWorkFrameCounter;
	private static final long	PROPAGATE_MODULUS	= 5;
	private CogbotAvatar myCogSimConf;

	public static VisionMonitorChannelImpl		theVMCI = null;

	SignalStation() {
		loadCogSimConf();
	}
	public static SignalStation getSignalStation() {
		if (theSignalStation == null) {
			theSignalStation = new SignalStation();
		}
		return theSignalStation;
	}
	private String getNullableParameter(Parameters roboParams, String namePath[]) {
		String	value = roboParams.getDescendantValue(namePath);
		if ((value != null) && value.equals("NULL")) {
			value = null;
		}
		return value;
	}
	public void readParameters(Parameters roboParams) {
	}
	public CueSpaceStub getCueSpace() {
		return myIntegroidFacade.getCueBroker();
	}
	public JobSpaceStub getJobSpace() {
		return myIntegroidFacade.getJobBroker();
	}	
	public ConvoidCueSpace getConvoidCueSpace() {
		return (ConvoidCueSpace) getCueSpace();
	}
	public AnimoidCueSpaceStub getAnimoidCueSpace() {
		return (AnimoidCueSpaceStub) getJobSpace();
	}	
	public void setIntegroidFacade (IntegroidFacade igf) {
		myIntegroidFacade = igf;
		setReactionProcessor(igf);
	}
	public IntegroidFacade getIntegroidFacade() {
		return myIntegroidFacade;
	}
	private void setReactionProcessor(ReactionProcessor rp) {
		myReactionProcessor = rp;
	}
	public AnimoidFacade getAnimoidFacade() {
		AnimoidFacade af = null;
		if (myIntegroidFacade != null) {
			af = myIntegroidFacade.getAnimoidFacade();
		}
		return af;
	}
	private void advanceNowCueAndProcessDecisions() {
		long startStamp = TimeUtils.currentTimeMillis();
		IntegroidHelpFuncs.advanceNowCue(myIntegroidFacade, 100L, 1.0);
		long midStamp = TimeUtils.currentTimeMillis();
		// scheduleDelayedProcessing();
        if(myIntegroidFacade != null){
            myIntegroidFacade.processAllDecisions();
        }
		long endStamp = TimeUtils.currentTimeMillis();
		long advanceDur = midStamp - startStamp;
		long processDur = endStamp - midStamp;
		theLogger.fine("Advance now cue started at " + startStamp + " and used " + advanceDur + " msec, then processAllDecisions used " +  processDur + " msec.");
	}
	public void workHardLater(final FaceModel fm) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				theLogger.finer("======================= Hard Work START");
				try {
					ensureMotionLoopConnected();
					if (myPersonResolver != null) {
						myPersonResolver.resolveAllState(fm);
					}
					advanceNowCueAndProcessDecisions();
					AnimoidFacade af = getAnimoidFacade();
					if (af != null) {
						af.rebalanceGazeJobs();
					}
					propagateSignals();
				} catch (Throwable t) {
					theLogger.log(Level.SEVERE, "Hard Worker caught exception.", t);
				}
				theLogger.finer("======================= Hard Work  END");
			}
		});
	}
	private void propagateSignals() {
		myWorkFrameCounter++;
		if ((myWorkFrameCounter % PROPAGATE_MODULUS) == 0) {
			theLogger.finer("Propagating signals on work frame #" + myWorkFrameCounter);
			setChanged();
			notifyObservers();
		} else {
			theLogger.finest("Skipping signal propagation on work frame #" + myWorkFrameCounter);
		}
	}
	public void logDetectedObjectImage(PortableImage pimg) {
		myDetectedImagePanel.setPortableImage(pimg);
		myDetectedImagePanel.repaint();
	}
	private void scheduleDelayedProcessing() {
		if (myReactionProcessor != null) {
			// Actual processing occurs via invokeLater to avoid deadlocks or lenghty blocking here.
			myReactionProcessor.processWhenSafe();
		}
	}
		
	public void visemeEvent(long streamNumber, int curViseme, int duration, byte flags, int nextViseme) {
		theLogger.log(Level.FINER, "SignalStation got viseme signal @" + TimeUtils.currentTimeMillis() 
			+ "[stream=" + streamNumber + ", curViseme=" + curViseme
			+ ", duration=" + duration + ", nextViseme=" + nextViseme + ", flags=" + flags);
		
		getAnimoidFacade().suggestViseme(curViseme, duration, flags, nextViseme);
	}

	public void bookMarkEvent(long streamNumber, String bookmarkLabel) {
	}	
	
	public void endInputStreamEvent(long streamNumber) {
	}

	public void startInputStreamEvent(long streamNumber) {
	}
	public void saySAPI_blockHack(String sapiBlock)  {
		doitCogsim("say", sapiBlock, true);
	}
	private synchronized void loadCogSimConf() {
		if (myCogSimConf == null) {
			try {
				myCogSimConf = CogbotService.getDefaultAvatar(MeneConfig.readPropertiesFile(Communicator.thePropsPath));
			} catch (Throwable t) {
				theLogger.log(Level.WARNING, "Failed to load CogSimConf", t);
			}
		}
	}
	public void doitCogsim(final String verb, final String details, final boolean debugFlag) {
		if (myCogSimConf != null) {
			new Thread(new Runnable() {
				public void run() {
					try {
						myCogSimConf.postActionReqToCogbot(verb, details, debugFlag);
					} catch (Throwable t) {
						theLogger.log(Level.WARNING, "Cannot send cogbot-doit command[" + verb + ", " + details + "]", t);
					}
				}
			}).start();
		}
	}
	public String fetchCogsimHeard(boolean debugFlag) {
		String lastHeardTxt = null;
		if (myCogSimConf != null) {
			try {
				lastHeardTxt = myCogSimConf.fetchLastThingWeHeard(true);
			} catch (Throwable t) {
				theLogger.log(Level.WARNING, "Failed to fetch cogsim-last-heard", t);
			}
		}
		return lastHeardTxt;
	}

	/*
	public void animationStarted(AnimationExecJob aej) {
...				myCJMXW.notifyJobPosted(aej);
	public void animationEnded(AnimationExecJob aej) {
		if (myAnimationEndThoughtCueName != null) {
			postThoughtAndScheduleDelayedProcessing(myAnimationEndThoughtCueName, 1.0);
	 */
	public void talkToScala() {
		/*
		System.out.println("Talking to scala through the $.MODULE$ path");
		com.hansonrobotics.freckbase.Client$.MODULE$.becomeSomewhatJiggy();
		System.out.println("Talking to scala through the special static forwarder path");
		com.hansonrobotics.freckbase.Client.becomeSomewhatJiggy();
		*/
	}
	// public 		java.sql.Connection myConn;


	public Long writeFaceObsToFreckbase(FaceObservation fobs, Long hypoID) throws Throwable {
		/*
		if (myFreckbaseFacade == null) {
			myFreckbaseFacade = FreckbaseFacade.initClientFacade();
		}
		 */
		PortableImage facePI = fobs.getFaceImage();
		byte[] faceImageBytes = facePI.getBytesWithoutHeader();
		// TODO:  let freckbase assign the hypoIDs.
		// TODO:  set using proper enum value
		String initialFBRecStatus = "FRESH";
		// Note that this width+height is usually *not* the same as the bound-rect on fobs,
		// because the sample is expanded in FaceModel.noticeFaceRectangle
		return myFreckbaseFacade.writeFaceObs(
				fobs.getTimeStampMsec(), hypoID, initialFBRecStatus,
				facePI.getWidth(), facePI.getHeight(),
				faceImageBytes);
	}
	public Long writeFaceHypoToFreckbase(FaceHypothesis fh) throws Throwable {
		Long hypoID = fh.getFreckbaseHypoID();
		Double diamPix  = fh.getDiameterPixels();
		if (hypoID == null) {
			hypoID = myFreckbaseFacade.writeFaceHypo();
			fh.setFreckbaseHypoID(hypoID);
		}
		return hypoID;
	}
	public void initClientFreckbaseFacade(String dbFilePath) {
		myFreckbaseFacade = FreckbaseFacade.initClientFacade(dbFilePath);
	}
	public FreckbaseFacade getFreckbaseFacade() {
		return myFreckbaseFacade;
	}
	public void doPreConfigSystemInit() throws Throwable {
		//IDynamixelFacade unusedDynamixelFacadeHandle = DynamixelFacade.getInstance();
	}
	public void ensureMotionLoopConnected() {
		IntegroidFacade igf = getIntegroidFacade();
		if ((igf != null) && (theVMCI != null)) {
			MotionCue mc = AwarenessHelpFuncs.getTheMotionCue(igf);
			if (mc != null) {
				PeakTracker pt = mc.fetchPeakTracker();
				if (pt != null) {
					MotionFilterObserver mfo = theVMCI.getMotionDetectObserver();
					if (mfo != null) {
						mfo.setPeakTracker(pt);
					}
				}
			}
		}
	}
}
