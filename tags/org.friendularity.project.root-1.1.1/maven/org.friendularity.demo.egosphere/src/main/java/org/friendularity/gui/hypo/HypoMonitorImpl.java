/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.hypo;



import java.util.Observable;





import org.friendularity.app.face.FaceBaseMonitorImpl;
import java.util.Observer;
import java.util.logging.Logger;


/**
 * @author Stu Baurmann
 */
public class HypoMonitorImpl extends FaceBaseMonitorImpl implements Observer  {
    private static Logger	theLogger = Logger.getLogger(HypoMonitorImpl.class.getName());

	private		HypoTableModel				myHTM;
	private		HypoDistanceTableModel		myHDTM;

	HypoMonitorImpl() {
		myHTM = new HypoTableModel(this);
		myHDTM = new HypoDistanceTableModel(this);
	}
	public HypoTableModel	getHypoTableModel() {
		return myHTM;
	}
	public HypoDistanceTableModel	getHypoDistanceTableModel() {
		return myHDTM;
	}
	public void update(Observable o, Object arg) {
		myHTM.update(o, arg);
		myHDTM.update(o, arg);
	}
}
