package org.friendularity.gui.freckle;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JPanel;

import org.cogchar.nwrap.core.EmptyEngine;
import org.cogchar.vision.RawVisionObserver;
import org.cogchar.vision.VisionCanvas;
import org.cogchar.vision.VisionFacade;

public class CameraPanel extends JPanel implements WindowListener {
	public CameraPanel(AbstractAction buttonAction) {
    	Box camBox = Box.createVerticalBox();
    	VisionCanvas visPanel = new VisionCanvas();
    	visPanel.setPreferredSize(new Dimension(320,240));
    	camBox.add(visPanel);
		camBox.add(new JButton(buttonAction));
		add(camBox);
		
		m_engine = new EmptyEngine();
		m_vision = new VisionFacade();
		m_vision.configure("HaarFaceDetectCascadeFile=" + Constants.cvHaarPath);
			// C:\\hri\\RobotControl\\deploy\\haarcascade_frontalface_alt.xml");
        m_rawObs = new RawVisionObserver(visPanel);
        m_vision.SetRawVisionObserver(m_rawObs);
        visPanel.SetObserver(m_rawObs);
        m_vision.Activate();
	}

	public void windowClosed(WindowEvent e) {
		m_vision.DeActivate();
	}
	
	public Image getImage() {
		return m_rawObs.getImage();
	}

	// other stupid window listener functions
	public void windowActivated(WindowEvent e) {}
	public void windowClosing(WindowEvent e) {}
	public void windowDeactivated(WindowEvent e) {}
	public void windowDeiconified(WindowEvent e) {}
	public void windowIconified(WindowEvent e) {}
	public void windowOpened(WindowEvent e) {} 
	
	private VisionFacade m_vision;
	private RawVisionObserver m_rawObs;
	private EmptyEngine m_engine;
}
