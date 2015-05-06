/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.egosphere;

import org.friendularity.app.jmxwrap.SignalStation;

import org.friendularity.gui.vision.VisionMonitorChannelImpl;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Observable;
import java.util.Observer;
import java.util.logging.Logger;
import javax.swing.JTextArea;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.zzz.oldboot.SubsystemImpl;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.friendularity.gaze.api.AnimoidGazeFacade;
/**
 *
 * @author Matthew Stevenson
 */
public class EgosphereMonitorImpl extends SubsystemImpl implements Observer, PropertyChangeListener {
    private static Logger	theLogger = Logger.getLogger(EgosphereMonitorImpl.class.getName());
    private VisionMonitorChannelImpl			myVMCI;
	private IntegroidFacade						myIGF;
	
    private EgosphereSwingPanel                 myDrawingPanel;
	private ScalingVisionPanel					myScalingPanel;
    private EgosphereMonitorBean	            myEgoBean;

	private	JTextArea							myGazeDetailTextArea;
	
    public EgosphereMonitorImpl() { }
	public void setVMCI(VisionMonitorChannelImpl vmci){
		if(myVMCI == null && vmci != null){
			myVMCI = vmci;
			myVMCI.getRawFrameProcessor().addObserver(this);
			myDrawingPanel.setImageSource(myVMCI.getRawFrameProcessor());
			myScalingPanel.setImageSource(myVMCI.getRawFrameProcessor());
		}
	}
	public void setEgoBean(EgosphereMonitorBean emb) {
		myEgoBean = emb;
		myEgoBean.addPropertyChangeListener(this);
	}
	public boolean setIGF(IntegroidFacade igf){
		if(igf == null || igf.getAnimoidFacade() == null){
			return false;
		}
		myIGF = igf;
		return myDrawingPanel.setIGF(myIGF);
	}
	public VisionMonitorChannelImpl getVMCI(){
		return myVMCI;
	}
    public void setDrawingPanel(EgosphereSwingPanel drawingPanel) {
        myDrawingPanel = drawingPanel;
		if(myVMCI != null){
			myDrawingPanel.setImageSource(myVMCI.getRawFrameProcessor());   
		}
		if(myIGF != null){
			myDrawingPanel.setIGF(myIGF);
		}
    }
    public void setScalingPanel(ScalingVisionPanel scalingPanel) {
        myScalingPanel = scalingPanel;
		if(myVMCI != null){
			myScalingPanel.setImageSource(myVMCI.getRawFrameProcessor());   
		}
    }
	public void setGazeDetailTextArea(JTextArea gazeDetailTextArea) {
		myGazeDetailTextArea = gazeDetailTextArea;
	}
	public void propertyChange(PropertyChangeEvent evt) {
		String propertyName = evt.getPropertyName();
		Object propertyValue = evt.getNewValue();
		theLogger.fine("ChannelImpl got property change:  " + propertyName + " := " + propertyValue);
		if (java.beans.Beans.isDesignTime()) {
			theLogger.fine("It's design time!  No further processing of event");
			return;
		}
		if(myVMCI == null){
			setVMCI((VisionMonitorChannelImpl)lookupSubsystem(VisionMonitorChannelImpl.class));
		}
		if(myIGF == null){
			// setIGF(((ThalamusMonitorImpl)lookupSubsystem(ThalamusMonitorImpl.class)).getIntegroidFacade());
		}
		if(myVMCI == null || myIGF == null || myIGF.getAnimoidFacade() == null){
			theLogger.warning("VMCI, TMI, or AF is null, cannot continue.");
			return;	
		}
		if (propertyName.equals(EgosphereMonitorBean.PROP_DISPLAY_VIDEO)) {
            if (propertyValue.equals(Boolean.TRUE)) {
				setVMCI(SignalStation.theVMCI);
				// This should not be necessary, and in fact it causes problems.
				// myVMCI.resetObserver();
            }
        }
    }
   public void update (Observable o, Object arg) {
        myDrawingPanel.repaint();
		myScalingPanel.repaint();
		AnimoidGazeFacade af = (AnimoidGazeFacade) myIGF.getAnimoidFacade();
		String debugText = af.getAttentionDebugText();
		myGazeDetailTextArea.setText(debugText);
    }
}