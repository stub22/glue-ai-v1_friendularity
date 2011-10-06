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
import org.cogchar.sight.hypo.SightHypoComparison;
import org.cogchar.sight.obs.SightObservationComparison;

/**
 *
 * @author Stu Baurmann
 */
public class HypoDistanceTableModel  extends AbstractTableModel implements Observer {
    private static Logger	theLogger = Logger.getLogger(HypoDistanceTableModel.class.getName());

	private		HypoMonitorImpl		myHMImpl;

	private		List<SightHypoComparison<FaceHypothesis>>	myCachedComparisons;
	
	public HypoDistanceTableModel(HypoMonitorImpl impl) {
		myHMImpl = impl;
	}

	private void refreshComparisons() {
		FaceModel fm = myHMImpl.getFaceModel();
		if (fm != null) {
			myCachedComparisons = fm.getCachedComparisons();
		} 
	}
	@Override public int getColumnCount() {
		return 7;
	}
	@Override public int getRowCount() {
		refreshComparisons();
		return (myCachedComparisons != null) ? myCachedComparisons.size() : 0;
	}
	@Override public String getColumnName(int columnIndex) {
		String cols[] = 
		{"Hypo A#", "Hypo B#", "Cog Dist", "Overlap Cnt", "Diff Seconds", "Diff Az Diam", "Diff El Diam"};
		return cols[columnIndex];
	}
	
	public Object getValueAt(int rowIndex, int columnIndex) {
		Object result = null;
		SightHypoComparison<FaceHypothesis> hc = myCachedComparisons.get(rowIndex);
		FaceHypothesis fha = hc.lowerHypo;
		FaceHypothesis fhb = hc.upperHypo;
		double cogDist = hc.getCognitiveDistance();
		SightObservationComparison soc = hc.getObservationComparison();
		int overlapCount = hc.myOverlapCount;
		switch(columnIndex) {
		case 0:
			result = fha.getHypothesisNumber();
		break;
		case 1:
			result = fhb.getHypothesisNumber();
		break;
		case 2:
			result = cogDist;
		break;
		case 3:
			result = overlapCount;
		break;
		case 4:
			result = soc.timeDiffSec;
		break;
		case 5:
			result = soc.azDiffDiams;
		break;
		case 6:
			result = soc.elDiffDiams;
		break;

		}
		return result;
	}

	public void update(Observable o, Object arg) {
		theLogger.finest("firing tableDataChanged");
		this.fireTableDataChanged();
	}
	
}
