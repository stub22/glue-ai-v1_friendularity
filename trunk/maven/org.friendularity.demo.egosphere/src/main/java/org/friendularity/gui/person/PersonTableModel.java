/*
 *  Copyright 2008-9 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.person;

import java.awt.Component;
import java.awt.Font;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
// Drools 4
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import org.cogchar.api.integroid.cue.PersonCue;
import org.cogchar.api.integroid.cue.PersonCue.NameSource;
import org.cogchar.zzz.platform.stub.ThalamusBrokerStub;
import org.cogchar.platform.util.TimeUtils;
import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.face.FreckledObsLog;
import org.friendularity.app.freckle.FreckleFace;
import org.friendularity.app.person.PersonHelpFuncs;
// import org.drools.FactHandle;

/**
 * @author Stu Baurmann
 */
public class PersonTableModel extends AbstractTableModel {
	private static Logger	theLogger = Logger.getLogger(PersonTableModel.class.getName());
		//"com.hansonrobotics.gui.fact.PersonTableModel");
	
	private	PersonMonitorImpl		myPersonMonitorImpl;
	private	ThalamusBrokerStub			mySourceBroker;

	private	List<PersonCue>			myCachedPersons;
	private	long					myLastRefreshStampMsec;
	private static String theColumnNames [] =  {
		"cueType / cueSessID / cuePermID / hypoNum",
		"frec match age / cnt / ID",
		"name / source",
		"grtd / inqd / inq-needed",
		"cue strength / eligibility",
		"last rec obs",
		"last hyp obs",
		"activ / expo",
		"atten / age"
	};
	
	public PersonTableModel(PersonMonitorImpl fmi) {
		myPersonMonitorImpl = fmi;
	}
	public void clearCache() {
		myCachedPersons = null;
	}
	public void setSourceBroker(ThalamusBrokerStub tb) {
		mySourceBroker = tb;
		clearCache();
	}
	protected void ensureInitialized() {
		if (mySourceBroker == null) {
			mySourceBroker = myPersonMonitorImpl.getFactSourceBroker();
			theLogger.fine("Got fact source broker: " + mySourceBroker);			
		}
	}
	protected synchronized void refresh() {
		ensureInitialized();
		if (mySourceBroker != null) {
			myCachedPersons = mySourceBroker.getAllFactsMatchingClass(PersonCue.class);
			Collections.sort(myCachedPersons, new Comparator<PersonCue>() {
				public int 	compare(PersonCue pc1, PersonCue pc2) {
					Integer pid1 = pc1.fetchSessionCueID();
					Integer pid2 = pc2.fetchSessionCueID();
					return pid1 - pid2;
				}
				public boolean 	equals(Object obj)  {
					return false;
				}
			});
			// Maybe sort by lastUpdateTimsestamp - descending?
			theLogger.finer("Refreshed pcue cache, now contains: " + myCachedPersons);
			myLastRefreshStampMsec = TimeUtils.currentTimeMillis();
		}
	}
	protected void refreshIfNeeded(long maxCacheLifeMsec) {
		if (myCachedPersons != null) {
			long	 nowMsec = TimeUtils.currentTimeMillis();
			if  (nowMsec - myLastRefreshStampMsec > maxCacheLifeMsec) {
				refresh();
			}
		} else {
			refresh();
		}
	}
	
	public int getColumnCount() {
		return 9;
	}
	public String getColumnName(int i) {
		return theColumnNames[i];
	}
	public int getRowCount() {
		refreshIfNeeded(100);
		if (myCachedPersons != null) {
			return myCachedPersons.size();
		} else {
			return 0;
		}
	}
	public Class getColumnClass(int c) {
		// The returned class will be used to pick a renderer for the column c.
		Class clz = null;
		if ((c == 5) || (c == 6)) {
			return FaceObservation.class;
		} else {
			return String.class;
		}
		/*
		Object val = getValueAt(0, c);
		if (val != null) {
			clz = val.getClass();
		}
		return clz;
		*/
	}
	private PersonCue getCueAtRow(int rowIndex) {
		if (myCachedPersons == null) {
			theLogger.warning("myCachedPersons is null");
			return null;
		}
		if (myCachedPersons.size() <= rowIndex) {
			theLogger.warning("myCachedPersons is smaller than requested index:" + rowIndex);
			return null;
		}
		PersonCue pcue = myCachedPersons.get(rowIndex);
		return pcue;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		// The type returned must match what is returned by getColumnClass above
		// (which is what determines the renderer type, when it is not explicitly set).

		// We are in danger of drawing cells from different snapshots!
		// TODO:  How could we cause resync to happen exactly once per table-redraw?
		refreshIfNeeded(100);
		PersonCue pcue = getCueAtRow(rowIndex);
		FreckleFace fface = myPersonMonitorImpl.getFreckleFaceForPersonCue(pcue);
		FaceHypothesis fhypo = myPersonMonitorImpl.getFaceHypoForPersonCue(pcue);
		String freckleID = null;
		Double freckleMatchAge = null;
		Integer faceNumber = null;
		String attentionStat = null;
		Double attentionStatAge = null;
		Integer freckleMatchCount = null;
		String cueType = null;
		if (pcue != null) {
			cueType = pcue.getClass().getSimpleName();
			if (fhypo != null) {
				faceNumber = fhypo.getHypothesisNumber();
			}
			if (fface != null) {
				freckleID = fface.getFreckleID();
				freckleMatchCount = fface.getMatchedObservationCount();
			}
			attentionStat = pcue.getAttentionStatus().name();
			attentionStatAge = pcue.getAttentionStatusAgeSec();
			freckleMatchAge = pcue.getPermPersonConfirmAgeSec();
		} else {
			return "Null pcue at row: " + rowIndex;
		}
		Object val = null;
		switch(columnIndex) {
			case 0:
				val = "" + cueType + " / " + pcue.fetchSessionCueID() + " / " + pcue.getPermPersonID() + " / " + faceNumber;
			break;
			case 1:
				val = "" + freckleMatchAge + " / " + freckleMatchCount + " / " + freckleID;
			break;
			case 2:
				String sn = pcue.getSpokenName();
				NameSource sns = pcue.getNameSource();
				String snss = (sns != null) ? sns.toString() : "null";
				val = "" + sn + " / " + sns;
			break;
			case 3:
				val = "" + pcue.getPersonalGreetingCount() + " / " + pcue.getNameInquiryCount() + " / " + pcue.getNameInquiryNeeded();
			break;
			case 4:
				val = "" + String.format("%8.6f", pcue.getStrength()) + " / "
						+  PersonHelpFuncs.formattedAttentionEligibility(pcue);
			break;
			case 5:
				if (fface != null) {
					FreckledObsLog fol = fface.getFreckledObsLog();
					FaceObservation lastFreckledObs = (FaceObservation) fol.getMostRecentObservation();
					val = lastFreckledObs;
				}
				// In this case we want to return a raw null on failure, rather than
				// a string "null", which cannot be cast to FaceObservation.
				return val;
			case 6:
				if (fhypo != null) {
					FaceObservation lastObs = (FaceObservation) fhypo.getMostAccurateObservation();
					val = lastObs;
				}
				// In this case we want to return a raw null on failure, rather than
				// a string "null", which cannot be cast to FaceObservation.
				return val;
			case 7:
				if (fhypo != null) {
					val = fhypo.getActivationStatus().toString()
							+ "/" + fhypo.getExposureStatus();
				}
			break;
			case 8:
				val = "" + attentionStat + " / " + attentionStatAge;
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
	/*
fireTableCellUpdated 	Update of specified cell.
fireTableRowsUpdated 	Update of specified rows
fireTableDataChanged 	Update of entire table (data only).
fireTableRowsInserted 	New rows inserted.
fireTableRowsDeleted 	Existing rows Deleted
fireTableStructureChanged   	Invalidate entire table, both data and structure.
	*/


	public void initRenderers(JTable table) {
		TableColumnModel tcm = table.getColumnModel();

		TableColumn freckledObsCol = tcm.getColumn(5);
		freckledObsCol.setCellRenderer(new FaceObsColumnRenderer());

		TableColumn hypoObsCol = tcm.getColumn(6);
		hypoObsCol.setCellRenderer(new FaceObsColumnRenderer());
		// This doesn't work...because we are using fireTableStructureChanged()?
		// TableColumn col = table.getColumnModel().getColumn(4);
		// col.setCellRenderer(fobsr);
		// So instead we do this, which works fine:
		// table.setDefaultRenderer(FaceObservation.class, fobsr);
		
		TableColumn freckleIdentCol = table.getColumnModel().getColumn(1);
		freckleIdentCol.setCellRenderer(new WrappingTextColumnRenderer());
		// table.setDefaultRenderer(String.class, wtr);
		initListeners(table);
	}
	public void initListeners(final JTable table) {
		table.setCellSelectionEnabled(true);
		ListSelectionModel cellSelectionModel = table.getSelectionModel();
		// Not possible to prevent multiple column selection?
		cellSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		cellSelectionModel.addListSelectionListener(new ListSelectionListener() {

			public void valueChanged(ListSelectionEvent e) {
				Object selectedData = null;

				int[] selectedRow = table.getSelectedRows();
				int[] selectedColumns = table.getSelectedColumns();

				for (int i = 0; i < selectedRow.length; i++) {
					for (int j = 0; j < selectedColumns.length; j++) {
						int selRow = selectedRow[i];
						int selCol = selectedColumns[j];
						selectedData = table.getValueAt(selRow, selCol);
						theLogger.info("PersonTable selected [r=" + selRow 
									+ ",c=" + selCol + "], data=" + selectedData);

						PersonCue pcue = getCueAtRow(selRow);
						if (selCol == 5) {
							FreckleFace ff = myPersonMonitorImpl.getFreckleFaceForPersonCue(pcue);
							myPersonMonitorImpl.setDetailedFreckleFace(ff);
						}
						if (selCol == 6) {
							FaceHypothesis hypo = myPersonMonitorImpl.getFaceHypoForPersonCue(pcue);
							myPersonMonitorImpl.setDetailedFaceHypothesis(hypo);
						}
					}
				}
			}
		});
	}

	static public class FaceObsColumnRenderer implements TableCellRenderer { // extends DefaultTableCellRenderer {
		private ArrayList<ObservationPanel> rowObsPanels = new ArrayList<ObservationPanel>();
		public Component getTableCellRendererComponent(JTable table, Object value, 
				boolean isSelected, boolean hasFocus, int row, int column) {
			ObservationPanel op = null;
			// theLogger.info("fetchingRenderer for row=" + row + ", col=" + column);
			// theLogger.info("value=" + value);
			while (rowObsPanels.size() < row + 1) {
				rowObsPanels.add(null);
			}
			op = rowObsPanels.get(row);
			if (op == null) {
				op = new ObservationPanel();
				rowObsPanels.set(row, op);
				/*
				final ObservationPanel fop = op;
				op.addMouseListener(new MouseAdapter() {
					public void mouseClicked(MouseEvent e) {
						// if (e.getClickCount() == 2) {
						// comp =  e.getComponent();
						Object esrc = e.getSource();
						theLogger.info("Got click event with source: " + esrc + " embedded in op: " + op);
					}
				});
				 */
			}
			FaceObservation fobs = (FaceObservation) value;
			op.setObservation(fobs);
			return op;
		}
	}
	static public class WrappingTextColumnRenderer implements TableCellRenderer { // extends DefaultTableCellRenderer {
		private ArrayList<JTextArea> rowTAs = new ArrayList<JTextArea>();
		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			JTextArea ta = null;
			// theLogger.info("fetchingRenderer for row=" + row + ", col=" + column);
			// theLogger.info("value=" + value);
			while (rowTAs.size() < row + 1) {
				rowTAs.add(null);
			}
			ta = rowTAs.get(row);
			if (ta == null) {
				ta = new JTextArea();
				Font littleFont = new Font("Monospaced", Font.PLAIN, 10);
				ta.setFont(littleFont);
				ta.setLineWrap(true);
				ta.setWrapStyleWord(false);
				// ta.setBackground(Color.BLUE);
				rowTAs.set(row, ta);
			}
			String displayString = (String) value;
			ta.setText(displayString);
			return ta;
		}
	}


}
