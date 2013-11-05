/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.freckle;

import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.jmxwrap.SignalStation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.ancient.utility.Parameters;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
import org.cogchar.api.freckler.protocol.FreckleMatchConfig;
import org.cogchar.api.freckler.protocol.FreckleQuery;
import org.cogchar.api.freckler.protocol.FreckleResult;
import org.cogchar.sight.hypo.SightHypothesis;
import org.freckler.jmxwrap.FreckleServiceClient;
import org.freckler.jmxwrap.FreckleServiceWrapper;
import org.freckler.jmxwrap.FreckleServiceWrapperMXBean;
import org.freckler.service.FreckleResultListener;
import org.freckler.service.FreckleServiceImpl;
/**
 * @author Stu B. <www.texpedient.com>
 */
public class FreckleMatcher implements  FreckleResultListener, Runnable {
	private static Logger theLogger = Logger.getLogger(FreckleMatcher.class.getName());

	private	static boolean						asyncMode = false;

	private		FreckleMatchBatch.Supplier		myMatchBatchSupplier;

	private		Thread							myThread;
	private		boolean							myStopRequest = false;
	private		int								myTotalMatchResultsProcessed = 0;
	private		FreckleServiceImpl				myFSI;
	private		FreckleServiceWrapperMXBean		myFreckleService;

	private		FreckleFaceInventory				myInventory;

	private		Map<String, FreckleMatchCandidate>	myCandsByHandle;

	private		boolean							myViabilityFlag = false;

	// private		FreckleFile						myFreckleFile;
	
	public FreckleMatcher() {
		myInventory = new FreckleFaceInventory();
		myCandsByHandle = new HashMap<String, FreckleMatchCandidate>();
	}
	public void setBatchSupplier(FreckleMatchBatch.Supplier supplier) {
		myMatchBatchSupplier = supplier;
	}
	public void setService(FreckleServiceWrapperMXBean serviceBean) {
		myFreckleService = serviceBean;
	}
	public FreckleFaceInventory getInventory() {
		return myInventory;
	}
	public void configureServerOrConnectProxy(Parameters visionParams) {
		myInventory.configure(visionParams);
		String fvConfigPath = visionParams.getChildValue("FaceVacsConfigFile");
		String fvRepositoryPath = visionParams.getChildValue("FaceVacsRepositoryFile");
		String freckleServiceURL = visionParams.getChildValue("FreckleServiceURL");
		String freckbaseFilePath = visionParams.getChildValue("FreckbaseFilePath");
		if ((fvConfigPath != null) && (fvRepositoryPath != null)) {
			// We are running Freckle service in-process
			myFSI = new FreckleServiceImpl(fvConfigPath, fvRepositoryPath);
			// Instantiate a wrapper+queue, but do not advertise JMX service.
			FreckleServiceWrapper fsw = new FreckleServiceWrapper(null);
			// Use a local service impl
			fsw.setServiceImpl(myFSI);
			// Get local asynchronous notices when results are ready.
			fsw.setLoopbackListener(this);
			myFreckleService = fsw;
			myViabilityFlag = true;
		} else if (freckleServiceURL != null) {
			// We are running as a JMX client.  No local service impl.
			FreckleServiceClient client = FreckleServiceClient.makeClientAndConnect(freckleServiceURL, true);
			client.addListener(this);
			myFreckleService = client;
			if (freckbaseFilePath != null) {
				SignalStation.getSignalStation().initClientFreckbaseFacade(freckbaseFilePath);
				myViabilityFlag = true;
			} else {
				theLogger.warning("No freckbase file path, so not initializing freckbase client facade");
			}
		} else {
			theLogger.warning("No freckle service URL, so not initializing freckler client");
		}
	}
	public void setupIfNeeded() {
		if (myFSI != null) {
			// Setup in-process connection to FaceVacs.
			// myFSI.connectToFreckleNativeImpl();
//	TODO - reconsider local service in context of Freckbase.
			myFSI.loadDefaultPopulation();
		}
		// This will do nothing if inventory was loaded previously.
		myInventory.loadKnownFaces();
	}
	public synchronized void startMatching() {
		// FIXME:  If user  stopped and then started again quickly, could wind
		// up with 2 threads.  Need additional state flag confirming that old
		// thread has stopped before allowing another start command to be accepted.
		myStopRequest = false;
		myThread = new Thread(this);
		myThread.start();
	}
	public synchronized void stopMatching() {
		myStopRequest = true;
	}
	public void run() {
		setupIfNeeded();
		while (!myStopRequest) {
			try {
				if (myViabilityFlag) {
					// This send is used for both in/out process and both sync/async.
					FreckleMatchBatch batch = sendQueriesForAllCandidates();
					if ((myFreckleService instanceof FreckleServiceWrapper) && asyncMode){
						// FreckleService is running in-process.  Allow it to work
						// on this thread.
						for (int i=0; i< batch.myCandidates.size(); i++) {
							FreckleServiceWrapper fsw = (FreckleServiceWrapper) myFreckleService;
							fsw.handleOneQueuedQuery();
						}
					}

					// We overdo this, because it is a pain to try to call it
					// when names CHANGE, as well as after successful results.
					// TODO:  Perhaps compute a hashcode over the entire inventory,
					// and write only when hashcode changes.
					myInventory.saveKnownFaces();
					int obsToKeepPerFace = SightHypothesis.getFaceNoticeConfig().obsRetainedInFreckleFace;
					myInventory.trimOldObservations(obsToKeepPerFace);
					rest(batch);
				} else {
					theLogger.info("FreckleMatcher not yet viable, sleeping for one sec.");
					Thread.sleep(1000);
				}

			} catch (Throwable t) {
				theLogger.log(Level.SEVERE, "problem in FreckleMatcher thread", t);
			}
		}
		if (myFSI != null) {
			// myFSI.disconnectFromFreckleService();
		}
	}


	public synchronized FreckleMatchBatch sendQueriesForAllCandidates() throws Throwable  {
		
		// Idea is that we try one obs-match for each hypothesis on each round, unless there are
		// no fresh images to try.  This algorithm does not super-prioritize the matching of hypotheses
		// with NO freckle data, which may be the next dev step.
		FreckleMatchBatch	batch = myMatchBatchSupplier.getFreckleMatchBatch();
		Collection<FreckleMatchCandidate> fmcColl = batch.myCandidates;
		if (fmcColl.size() > 0) {
			theLogger.fine("FreckleMatcher checking " + fmcColl.size() + " candidates");
			for (FreckleMatchCandidate fmc: fmcColl) {
				sendQueryForCandidate(fmc);
				// Either during the send call above (in-process, synchronous), 
				// or at some future point, we receive a result in noticeFreckleResult.
			}
		}
		return batch;
	}
	private void sendQueryForCandidate(FreckleMatchCandidate fmc) throws Throwable {
		SignalStation ss = SignalStation.getSignalStation();
		FaceHypothesis	hypo = fmc.getHypothesis();
		Long hypoID = ss.writeFaceHypoToFreckbase(hypo);
		// TODO:  Make configurable
		FaceObservation fobs = fmc.getObservationToTry();
		Long freckbaseObsID = ss.writeFaceObsToFreckbase(fobs, hypoID);

		AnimoidFacade af = ss.getAnimoidFacade();
		AnimoidConfig ac = af.getAnimoidConfig();
		FreckleMatchConfig matchConfig = ac.getFreckleMatchConfig();

		FreckleQuery fq = fmc.makeFreckleQuery(freckbaseObsID,matchConfig);
		myCandsByHandle.put(fq.getHandle(), fmc);
		if (asyncMode) {
			myFreckleService.submitAsyncQuery(fq);
		} else {
			// We discard the returned result, and allow it instead to be
			// received in the async handler below.
			myFreckleService.syncQuery(fq, true);
		}
	
	}
	public synchronized void noticeFreckleResult(FreckleResult fres) {
		// theLogger.info("Processing freckleResult: " + fres.toString());
		// We update the freckle population first, so that any new faces
		// are available for enrolling.
		String[] popFreckleIDs = fres.getPopulationFreckleIDs();
		if (popFreckleIDs != null) {
			boolean inventoryChanged = myInventory.updateKnownFaces(popFreckleIDs);
			// Moved regular save into main loop, since there's lots of different times
			// we save.
			// if (inventoryChanged) {
			// 	myInventory.saveKnownFaces();
			// }
		}
		FreckleMatchCandidate fmc = myCandsByHandle.remove(fres.getSubmittedHandle());
		if (fmc == null) {
			theLogger.severe("Can't find FreckleMatchCandidate for handle: " + fres.getSubmittedHandle());
			return;
		}
		// TODO:  Make configurable
		double MINIMUM_MATCH_SCORE_ACCEPTED = 0.6;
		fmc.recordFreckleResult(fres, myInventory, MINIMUM_MATCH_SCORE_ACCEPTED);
		myTotalMatchResultsProcessed++;

		theLogger.fine("Attempt " + myTotalMatchResultsProcessed + " yielded "
						+ fmc.getObservationToTry().getRecognitionStatus() + " and ID=" + fmc.getResultFreckleID());

		if (myFSI != null) {
			myFSI.saveDefaultPopulation();
		}
	}

	public void rest(FreckleMatchBatch	batch) {
		// MemoryUtils.forceGarbageCollection(true);
		//long postGCstamp = TimeUtils.currentTimeMillis();
		//long gcElapsed = postGCstamp - endStamp;
		//theLogger.fine("******************GarbageCollect ended at " + postGCstamp + " and used " + gcElapsed + " msec");
		Long sleepTimeMillisec = batch.mySleepMillisec;
		theLogger.finer("Sleeping for " + sleepTimeMillisec);
		try {
			Thread.sleep(sleepTimeMillisec);
		} catch (Throwable t) {
			theLogger.log(Level.WARNING, "sleep interrupted by exception", t);
			t.printStackTrace();
		}			
	}
}
