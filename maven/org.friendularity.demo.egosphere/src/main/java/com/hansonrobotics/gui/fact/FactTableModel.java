/*
 *  Copyright 2008-9 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package com.hansonrobotics.gui.fact;


import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.table.AbstractTableModel;
// Drools 4
import org.cogchar.platform.stub.CueStub;
import org.cogchar.platform.stub.FactHandleStub;
import org.cogchar.platform.stub.JobStub;
import org.cogchar.platform.stub.ThalamentStub;
import org.cogchar.platform.stub.ThalamusBrokerStub;
import org.cogchar.platform.util.TimeUtils;
// import org.drools.FactHandle;

/**
 * @author Stu Baurmann
 */
public class FactTableModel extends AbstractTableModel {
	private static Logger	theLogger = Logger.getLogger(FactTableModel.class.getName());
		//"com.hansonrobotics.gui.fact.FactTableModel");	
	
	private	FactMonitorImpl		myFactMonitorImpl;
	private	ThalamusBrokerStub		mySourceBroker;
	private	List<FactHandleStub>	myCachedResult;
    private List<FactHandleStub>    myFilteredCache;
	private	long				myLastRefreshStampMsec;
    private List<String>        myFilters;
	
	public FactTableModel(FactMonitorImpl fmi) {
		myFactMonitorImpl = fmi;
	}
	public void clearCache() {
		myCachedResult = null;
	}
	public void setSourceBroker(ThalamusBrokerStub tb) {
		mySourceBroker = tb;
		clearCache();
	}
    public void setFilters(String filterStr){
        myFilters = new ArrayList<String>();
        String[] filters = filterStr.split(",");
        for(String s : filters){
            s = s.trim();
            if(!s.isEmpty()){
                myFilters.add(s);
            }
        }
        filterCache();
    }
    private void filterCache(){
        if (myCachedResult == null) {
            return;
        }
        if(myFilters == null || myFilters.isEmpty()){
            myFilteredCache = myCachedResult;
            return;
        }
        List<Class> keep = new ArrayList();
        List<Class> ignore = new ArrayList();
        myFilteredCache = new ArrayList();
        for(FactHandleStub fh : myCachedResult){
            Object factObj = mySourceBroker.getFactObjectFromHandle(fh);
            Class c = factObj.getClass();
            if(keep.contains(c)){
                myFilteredCache.add(fh);
                continue;
            }
            if(ignore.contains(c)){
                continue;
            }
            if(classIncluded(c.getSimpleName())){
                keep.add(c);
                myFilteredCache.add(fh);
            }else{
                ignore.add(c);
            }
        }
    }

    private boolean classIncluded(String clss){
        for(String filter : myFilters){
            if(clss.matches(".*" + filter + ".*")){
                return true;
            }
        }
        return false;
    }
	protected void ensureInitialized() {
		if (mySourceBroker == null) {
			mySourceBroker = myFactMonitorImpl.getFactSourceBroker();
			theLogger.fine("Got fact source broker: " + mySourceBroker);			
		}
	}
	protected void refresh() {
		ensureInitialized();
		if (mySourceBroker != null) {
			myCachedResult = mySourceBroker.getAllFactHandlesMatchingClass(ThalamentStub.class);
			// Maybe sort by lastUpdateTimsestamp - descending?
			theLogger.finer("Got cached result: " + myCachedResult);	
			myLastRefreshStampMsec = TimeUtils.currentTimeMillis();
            filterCache();
		}
	}
	protected void refreshIfNeeded(long maxCacheLifeMsec) {
		if (myCachedResult != null) {
			long	 nowMsec = TimeUtils.currentTimeMillis();
			if  (nowMsec - myLastRefreshStampMsec > maxCacheLifeMsec) {
				refresh();
			}
		} else {
			refresh();
		}
	}
	
	public int getColumnCount() {
		refreshIfNeeded(100);
		return 5;
	}

	public int getRowCount() {
		refreshIfNeeded(100);
		if (myFilteredCache != null) {
			return myFilteredCache.size();
		} else {
			return 0;
		}
	}
	static String theColNames[] = {"type", "age", "age-update", "strength/status", "content summary"};
	static Class theColClasses[] = {String.class, Double.class, Double.class, String.class, String.class};

	public String getColumnName(int cidx) {
		return theColNames[cidx];
	}
	@Override public Class getColumnClass(int colIndex) {
		return theColClasses[colIndex];
	}
	public Double nonNull(Double obj, Double def) {
		if (obj == null) {
			return def;
		} else {
			return obj;
		}
	}
	public Object getValueAt(int rowIndex, int columnIndex) {
		refreshIfNeeded(100);
		if (myFilteredCache == null) {
			return null;
		}
		FactHandleStub fh = myFilteredCache.get(rowIndex);
		Object factObj = mySourceBroker.getFactObjectFromHandle(fh);
		ThalamentStub thal = null;
		if (factObj instanceof ThalamentStub) {
			thal = (ThalamentStub) factObj;
		} else if (factObj == null) {
			return "null fact (race)";
		} else {
			return "NonThalament[" + factObj.getClass().getSimpleName() + "]";
		}
		switch(columnIndex) {
			case 0:
				return thal.getTypeString();
			case 1:
				return nonNull(TimeUtils.getStampAgeSec(thal.getCreateStampMsec()), -999.99);
			case 2:
				return nonNull(TimeUtils.getStampAgeSec(thal.getUpdateStampMsec()), -999.99);
			case 3:
				if (thal instanceof CueStub) {
					return "" + ((CueStub) thal).getStrength();
				} else if (thal instanceof JobStub) {
					return "" + ((JobStub) thal).getStatusString();
				}
			case 4:
				return "" + thal.getContentSummaryString();
			default:
				return "val[" + rowIndex + "," + columnIndex + "]";
		}
	}
	public void invalidateEverything() {
		// theLogger.info("Firing tableDataChanged event");
		// fireTableStructureChanged();
		fireTableDataChanged();
	}
	/*
fireTableCellUpdated 	Update of specified cell.
fireTableRowsUpdated 	Update of specified rows
fireTableDataChanged 	Update of entire table (data only).
fireTableRowsInserted 	New rows inserted.
fireTableRowsDeleted 	Existing rows Deleted
fireTableStructureChanged   	Invalidate entire table, both data and structure.	
	*/

}
