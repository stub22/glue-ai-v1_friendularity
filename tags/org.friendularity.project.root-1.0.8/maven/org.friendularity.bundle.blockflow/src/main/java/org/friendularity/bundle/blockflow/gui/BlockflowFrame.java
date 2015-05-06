
package org.friendularity.bundle.blockflow.gui;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.JFrame;
import org.friendularity.bundle.blockflow.engine.BlockflowEngine;
import org.friendularity.bundle.blockflow.engine.Quitter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *  Main frame for Blockflow
 * 
 * This frame delegates most of it's behavior to BlockflowPanel
 * 
 * @author Annie
 */

public class BlockflowFrame extends JFrame implements WindowListener {
	static Logger theLogger = LoggerFactory.getLogger(org.friendularity.jvision.gui.JVisionFrame.class);

	private static final long serialVersionUID = -242343530233038194L;

	private	Quitter			myQuitter;
	
	private BlockflowPanel  mainPanel;

	@SuppressWarnings("LeakingThisInConstructor")

	BlockflowFrame(BlockflowEngine myEngine) {
		
		this.setSize(800, 640);
		this.setUndecorated(true);
		this.setResizable(false);
		
		this.setTitle("Blockflow");

		this.addWindowListener(this);
		
		mainPanel = new BlockflowPanel(myEngine);
		
		this.getContentPane().add(mainPanel);
		
		// We "do nothing" from Swing point of view, but still catch the 
		// WindowClosing event, which starts our official "quit" process.
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		// Other option is DISPOSE_ON_CLOSE
		// Useful in standalone java apps, but not super-kosher under OSGi:
		// this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		this.setVisible(true);
	}
	
	public void setQuitter(Quitter q) {
		myQuitter = q;
	}
	
	// ========================  Window Listeners  ========================
	@Override public void windowActivated(WindowEvent arg0) {
		
	}
	@Override public void windowClosed(WindowEvent arg0) {
		
	}
	@Override public void windowClosing(WindowEvent arg0) {
		theLogger.debug("Caught windowClosing() event");
		if (myQuitter != null) {
			theLogger.info("Setting wantsToQuit flag");
			myQuitter.setWantsToQuit(true);
		}
	}
	
	@Override public void windowDeactivated(WindowEvent arg0) {
		// TODO Auto-generated method stub
	}
	@Override public void windowDeiconified(WindowEvent arg0) {
		// TODO Auto-generated method stub
	}
	@Override	public void windowIconified(WindowEvent arg0) {
		// TODO Auto-generated method stub
	}
	@Override public void windowOpened(WindowEvent arg0) {
		// TODO Auto-generated method stub
	}
}
