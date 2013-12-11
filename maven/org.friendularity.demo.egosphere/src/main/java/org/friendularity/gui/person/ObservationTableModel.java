/*
 *  Copyright 2008-9 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.person;

import org.friendularity.gui.person.PersonTableModel.FaceObsColumnRenderer;
import org.friendularity.gui.person.PersonTableModel.WrappingTextColumnRenderer;
import java.util.ArrayList;
import java.util.List;
import java.util.NavigableSet;
import java.util.logging.Logger;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
// Drools 4
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.sight.api.facerec.FaceRecognitionStatus;
import org.cogchar.platform.util.TimeUtils;
import org.freckler.sight.impl.obs.SightObservationLog;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.freckle.FreckleFace;
// import org.drools.FactHandle;

/**
 * @author Stu Baurmann
 */
public class ObservationTableModel extends AbstractTableModel {
	private static Logger	theLogger = Logger.getLogger(ObservationTableModel.class.getName());

	private	PersonMonitorImpl		myPersonMonitorImpl;

	private	List<FaceObservation>	myCachedObservations;
	private	long					myLastRefreshStampMsec;

	private SightObservationLog		myObsLog;
	private static String theColumnNames [] =  {
		"image",
		"age / stamp",
		"azim / elev",
		"area / diam",
		"frec status",
		"samp qual",
		"freckleID",
		"freckle strength",
	};
	public ObservationTableModel(PersonMonitorImpl fmi) {
		myPersonMonitorImpl = fmi;
	}
	public void clearCache() {
		myCachedObservations = null;
	}
	public void setObservationLog(SightObservationLog sol) {
		myObsLog = sol;
		clearCache();
	}
	protected void ensureInitialized() {
	}
	protected void refresh() {
		ensureInitialized();
		myCachedObservations = null;
		if (myObsLog != null) {
			myCachedObservations = new ArrayList<FaceObservation>();
			NavigableSet<Long> recentStampNavSet = myObsLog.getNavigableRecentTimestamps();
			if (recentStampNavSet != null) {
				for (Long stamp: recentStampNavSet) {
					FaceObservation fobs = (FaceObservation) myObsLog.getObservationForTimestamp(stamp);
					if (fobs != null) {
						myCachedObservations.add(fobs);
					} else {
						theLogger.warning("Cannot fetch obs for timestamp: " + stamp + " - race condition?");
					}
				}
			}
			myLastRefreshStampMsec = TimeUtils.currentTimeMillis();
		}
	}
	protected void refreshIfNeeded(long maxCacheLifeMsec) {
		if (myCachedObservations != null) {
			long	 nowMsec = TimeUtils.currentTimeMillis();
			if  (nowMsec - myLastRefreshStampMsec > maxCacheLifeMsec) {
				refresh();
			}
		} else {
			refresh();
		}
	}

	public int getColumnCount() {
		return theColumnNames.length;
	}
	public String getColumnName(int i) {
		return theColumnNames[i];
	}
	public int getRowCount() {
		refreshIfNeeded(100);
		if (myCachedObservations != null) {
			return myCachedObservations.size();
		} else {
			return 0;
		}
	}
	public Class getColumnClass(int c) {
		// The returned class will be used to pick a renderer for the column c.
		Class clz = null;
		if (c == 0) {
			return FaceObservation.class;
		} else {
			return String.class;
		}
	}
	public Object getValueAt(int rowIndex, int columnIndex) {
		// The type returned must match what is returned by getColumnClass above
		// (which is what determines the renderer type).
		refreshIfNeeded(100);
		if (myCachedObservations == null) {
			theLogger.warning("myCachedObservations is null");
			return null;
		}
		if (myCachedObservations.size() <= rowIndex) {
			theLogger.warning("myCachedObservations is smaller than requested index:" + rowIndex);
			return null;
		}
		FaceObservation fobs = myCachedObservations.get(rowIndex);
		Long timestamp;
		double ageSec;
		String freckleID = null;

		FreckleFace fface = null;
		Double freckleMatchStrength = null;

		String az, el;
		Double pixelArea;
		Double pixelDiam;
		String qualSummary;

		FaceRecognitionStatus frecStatus;
		if (fobs != null) {
			timestamp = fobs.getTimeStampMsec();
			ageSec = fobs.getAgeSec();
			fface = fobs.getFreckleFace();
			if (fface != null) {
				freckleID = fface.getFreckleID();
			}
			freckleMatchStrength = fobs.getFreckleMatchStrength();
			EgocentricDirection centerDir = fobs.getCenterDirection();
			az = centerDir.getAzimuth().getDegreesText();
			el = centerDir.getElevation().getDegreesText();
			pixelArea = fobs.getPixelArea();
			pixelDiam = fobs.getDiameterPixels();
			frecStatus = fobs.getRecognitionStatus();
			qualSummary = fobs.getFreckleSampleQualitySummary();
		} else {
			return "Null pcue at row: " + rowIndex;
		}
		Object val = null;
		switch(columnIndex) {
			case 0:
				val = fobs;
			break;
			case 1:
				val = "" + ageSec + " / " + timestamp;
			break;
			case 2:
				val = "" + az + " / " + el;
			break;
			case 3:
				val = "" + pixelArea + " / " + pixelDiam;
			break;
			case 4:
				val = frecStatus;
			break;
			case 5:
				val = qualSummary;
			break;
			case 6:
				val = freckleID;
			break;
			case 7:
				val = freckleMatchStrength;
			break;
			default:
				val = "val[" + rowIndex + "," + columnIndex + "]";
			break;
		}
		if (val == null) {
			return "null";
		} else if (val instanceof Number) {
			// We don't want to return the Number, because then we must ALWAYS return a number
			// for that column.
			// (So we can't return "" or "none" or "null", without getting an exception).
			return val.toString();
		} else {
			return val;
		}
	}
	public void invalidateEverything() {
		theLogger.info("firing tableStructureChanged event");
		fireTableStructureChanged();
	}

	public void initRenderers(JTable table) {
		TableColumnModel tcm = table.getColumnModel();

		TableColumn freckledObsCol = tcm.getColumn(0);
		freckledObsCol.setCellRenderer(new FaceObsColumnRenderer());

		TableColumn qualSummCol = table.getColumnModel().getColumn(5);
		qualSummCol.setCellRenderer(new WrappingTextColumnRenderer());

		TableColumn freckleIdentCol = table.getColumnModel().getColumn(6);
		freckleIdentCol.setCellRenderer(new WrappingTextColumnRenderer());

	}
	

}
