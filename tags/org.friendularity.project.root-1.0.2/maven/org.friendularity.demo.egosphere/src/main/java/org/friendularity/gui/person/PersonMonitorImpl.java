/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.person;

import java.util.Observable;
import java.util.logging.Logger;
import javax.swing.JTable;
import org.freckler.sight.impl.obs.SightObservationLog;
import org.friendularity.app.face.FaceBaseMonitorImpl;
import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import org.friendularity.app.freckle.FreckleFace;

/**
 *
 * @author Stu Baurmann
 */
public class PersonMonitorImpl extends FaceBaseMonitorImpl {
	private static Logger	theLogger = Logger.getLogger(PersonMonitorImpl.class.getName());

	private PersonMonitorPanel			myPMP;
	private	PersonTableModel			myPersonTableModel;
	private ObservationTableModel		myObsTableModel;


	public PersonMonitorImpl(PersonMonitorPanel panel, JTable personTable, JTable obsTable) {
		myPMP = panel;
		myPersonTableModel = new PersonTableModel(this);
		personTable.setModel(myPersonTableModel);
		myObsTableModel = new ObservationTableModel(this);
		obsTable.setModel(myObsTableModel);
		myPersonTableModel.initRenderers(personTable);
		myObsTableModel.initRenderers(obsTable);
	}

	public void updateTables() {
		// Hack to force registration as observer of FaceModel.
		FaceModel fm = getFaceModel();
		myPersonTableModel.fireTableDataChanged();
		myObsTableModel.fireTableDataChanged();
	}
	public void update(Observable o, Object arg) {
		updateTables();
	}
	private void setDetailedObsLog(SightObservationLog sol) {
		myObsTableModel.setObservationLog(sol);
	}
	public void setDetailedFreckleFace(FreckleFace ff) {
		String type = "NONE";
		String freckleID = "";
		String count = "";
		SightObservationLog sol = null;

		if (ff != null) {
			sol = ff.getFreckledObsLog();
			type = "Freckled Obs Log";
			freckleID = ff.getFreckleID();
			count = "" + ff.getRetainedObservationCount() + " / " + ff.getMatchedObservationCount();
		}
		myPMP.setObsLogInfo(type, freckleID, count);
		setDetailedObsLog(sol);
		updateTables();
	}
	public void setDetailedFaceHypothesis(FaceHypothesis hypo) {
		String type = "NONE";
		String hypoNum = "";
		String count = "";
		SightObservationLog sol = null;

		if (hypo != null) {
			sol = hypo;
			type = "Face Hypothesis";
			hypoNum = "" + hypo.getHypothesisNumber();
			count = "" + hypo.getRetainedObservationCount() + " / " + hypo.getTotalObservationCount();
		}
		myPMP.setObsLogInfo(type, hypoNum, count);
		setDetailedObsLog(sol);
		updateTables();
	}

}
