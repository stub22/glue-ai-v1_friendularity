/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.hypo;

import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.logging.Logger;
import javax.swing.table.AbstractTableModel;

/**
 *
 * @author Stu Baurmann
 */
public class HypoTableModel  extends AbstractTableModel implements Observer {
    private static Logger	theLogger = Logger.getLogger(HypoTableModel.class.getName());

	private		HypoMonitorImpl		myHMImpl;
	private		FaceModel			myFaceModel;
	public HypoTableModel(HypoMonitorImpl impl) {
		// Assume that impl will send us update notifications
		myHMImpl = impl;
	}
	public HypoTableModel(FaceModel fm) {
		myFaceModel = fm;
		myFaceModel.registerObserver(this);
	}
	private FaceModel getFaceModel() { 
		if (myFaceModel == null) {
			myFaceModel = myHMImpl.getFaceModel();
		}
		return myFaceModel;
	}
	@Override public int getColumnCount() {
		return 8;
	}
	public List<FaceHypothesis> getHypoList() {
		FaceModel fm = getFaceModel();
		if (fm != null) {
			return fm.getHypoSnapshotOrderedByNum();
		} else {
			return null;
		}
	}
	@Override public int getRowCount() {
		List<FaceHypothesis> shs = getHypoList();
		return (shs != null) ? shs.size() : 0;
	}
	@Override public String getColumnName(int columnIndex) {
		String cols[] = 
		{"Number", "# Obs Total/Retained", "Obs. Timestamp", "Strength",
					"Freck ID / Strength", "Freck Succ/Tries",
					"Activation", "Diam Deg/Pix"};
		return cols[columnIndex];
	}
	
	public Object getValueAt(int rowIndex, int columnIndex) {
		Object result = null;
		List<FaceHypothesis> shs = getHypoList();
		if (rowIndex < shs.size()) {
			FaceHypothesis fh = shs.get(rowIndex);
			if (fh != null) {
				switch(columnIndex) {
				case 0:
					result = fh.getHypothesisNumber();
				break;
				case 1:
					result = "" + fh.getTotalObservationCount() + " / " + fh.getRetainedObservationCount();
				break;
				case 2:
					result = fh.getLatestObservationTimeStamp();
				break;
				case 3:
					result = fh.getStrength();
				break;
				case 4:
					result = "" + fh.getLikelyFriendPermCueID() + " / " + fh.getLikelyFreckleStrength();
				break;
				case 5:
					result = "" + fh.getFreckleMatchSuccessCount() + " / " + fh.getFreckleMatchAttemptCount();
				break;
				case 6:
					result = fh.getActivationStatus();
				break;
				case 7:
					result = "" + fh.getDiameterDegrees() + "/" + fh.getDiameterPixels();
				break;
				}
			}
		}
		return result;
	}

	public void update(Observable o, Object arg) {
		theLogger.finest("firing tableDataChanged");
		this.fireTableDataChanged();
	}
	
}
