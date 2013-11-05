/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.freckle;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.ancient.utility.Parameters;
import org.jdesktop.observablecollections.ObservableCollections;
import org.jdesktop.observablecollections.ObservableList;

/**
 *
 * @author Stu Baurmann
 */
public class FreckleFaceInventory {
	private static Logger theLogger = Logger.getLogger(FreckleFaceInventory.class.getName());

	protected String myFreckleFilePath;
	protected ObservableList<FreckleFace> myKnownFaces;
	private		boolean		myLoadedFlag = false;

	public FreckleFaceInventory() {
		List<FreckleFace>	innerKFL = new ArrayList<FreckleFace>();
		myKnownFaces = ObservableCollections.observableList(innerKFL);
	}
	public void configure(Parameters visionParams) {
		myFreckleFilePath = visionParams.getChildValue("FreckleFile");
	}
	public synchronized FreckleFace getKnownFace(String freckleID) {
		if (freckleID != null) {
			for (FreckleFace ff : myKnownFaces) {
				if (ff.getFreckleID().equals(freckleID)) {
					return ff;
				}
			}
		}
		return null;
	}

	public ObservableList<FreckleFace> getObservableKnownFaceList() {
		return myKnownFaces;
	}

	public synchronized void loadKnownFaces() {
		if (myLoadedFlag) {
			theLogger.warning("Skipping freckle file load - already loaded!");
			return;
		}
		try {
			myKnownFaces.clear();
			theLogger.info("Calling FreckleFile.load(" + myFreckleFilePath + ")");
			FreckleFile ffile = FreckleFile.load(myFreckleFilePath);
			List<FreckleFace> faceList = ffile.getFaceList();
			theLogger.info("Got faceList of size: " + faceList.size());
			if (faceList != null) {
				for (FreckleFace ff : ffile.getFaceList()) {
					theLogger.info("Found stored freckleFace: " + ff);
					myKnownFaces.add(ff);
				}
			}
			myLoadedFlag = true;
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "problem loading freckle-faces from " + myFreckleFilePath, t);
		}
	}

	public synchronized void saveKnownFaces() {
		FreckleFile ffile = new FreckleFile();
		for (FreckleFace fface : myKnownFaces) {
			ffile.addFace(fface);
		}
		try {
			ffile.save(myFreckleFilePath);
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "problem saving freckle-faces to " + myFreckleFilePath, t);
		}
	}

	protected synchronized boolean updateKnownFaces(String[] faceIDs) {
		boolean changedFlag = false;
		for (String fid : faceIDs) {
			FreckleFace ff = getKnownFace(fid);
			if (ff == null) {
				theLogger.info("Constructing new freckle face for ID: " + fid);
				ff = new FreckleFace();
				ff.setFreckleID(fid);
				myKnownFaces.add(ff);
				changedFlag = true;
			}
		}
		Iterator<FreckleFace> ffi = myKnownFaces.iterator();
		while (ffi.hasNext()) {
			FreckleFace ff = ffi.next();
			String ffid = ff.getFreckleID();
			boolean found = false;
			for (String lfid : faceIDs) {
				if (lfid.equals(ffid)) {
					found = true;
					break;
				}
			}
			if (!found) {
				theLogger.info("Purging \'knownFace\' ID, which is not matched in native freckle population: " + ffid);
				ffi.remove();
				changedFlag = true;
			}
		}
		return changedFlag;
	}
	public synchronized void trimOldObservations(int obsToKeepPerFace) {
		for (FreckleFace fface : myKnownFaces) {
			fface.trimOldObservations(obsToKeepPerFace);
		}
	}
}
