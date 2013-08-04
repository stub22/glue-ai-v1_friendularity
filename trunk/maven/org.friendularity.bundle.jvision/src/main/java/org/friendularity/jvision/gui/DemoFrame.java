package org.friendularity.jvision.gui;


import org.friendularity.jvision.filters.FaceDetector;
import org.friendularity.jvision.filters.Blur;
import org.friendularity.jvision.filters.Grayscale;
import org.friendularity.jvision.filters.FilterSequence;
import org.friendularity.jvision.filters.Dilate;
import org.friendularity.jvision.filters.ProfileDetector;
import org.friendularity.jvision.filters.Erode;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import static javax.swing.JFrame.EXIT_ON_CLOSE;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import org.friendularity.jvision.engine.Displayer;
import org.friendularity.jvision.engine.Quitter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DemoFrame extends JFrame implements WindowListener, Displayer {
	static Logger theLogger = LoggerFactory.getLogger(DemoFrame.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = -2429127330233038194L;

	private ImageIcon		myImageOutIcon = new ImageIcon();
	private JButton			myImageOutButton = new JButton();
	private	Quitter			myQuitter;

	
	private JPanel			myControlsPanel = new JPanel();
	private JLabel			myLabel_Framerate = new JLabel();
	private JMenuBar		myMenuBar;
	
	// this is just protection, if we have no other we'll use this one
	private	FilterSequence	myBackupFilterSeq = new FilterSequence();
	
	public DemoFrame()
	{
		this.setSize(640, 640);
		this.setTitle("JVision Bundle - OpenCV java demo");
		
		myImageOutButton.setIcon(myImageOutIcon);
		myImageOutButton.setMinimumSize(new Dimension(640, 480));
		myImageOutButton.setSize(new Dimension(640, 480));
		
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(myImageOutButton, BorderLayout.CENTER);

		myControlsPanel.setMinimumSize(new Dimension(640, 160));
		myControlsPanel.setBackground(new Color(255, 200, 128));
		myControlsPanel.add(myLabel_Framerate);
		this.getContentPane().add(myControlsPanel, BorderLayout.PAGE_END);
		
		setupMenus();

		registerWindowListeners();
		
		this.setVisible(true);
		
		// We "do nothing" from Swing point of view, but still catch the 
		// WindowClosing event, which starts our official "quit" process.
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		// Other option is DISPOSE_ON_CLOSE
		// Useful in standalone java apps, but not super-kosher under OSGi:
		// this.setDefaultCloseOperation(EXIT_ON_CLOSE);
	}
	public void setQuitter(Quitter q) {
		myQuitter = q;
	}
	public void setControlledFilterSequence(FilterSequence filters) {
		if(filters == null)throw new IllegalArgumentException("dont set the controlled filters to nothing");
		this.myBackupFilterSeq = filters;		
	}

	@Override public void setDisplayedImage(BufferedImage img){
		myImageOutIcon.setImage(img);
		this.repaint();
	}
	
	@Override public void setFramerateMessage(String string) {
		myLabel_Framerate.setText(string);
	}
	
	// ========================  Window Listeners  ========================
	@Override public void windowActivated(WindowEvent arg0) {
		// TODO Auto-generated method stub
	}
	@Override public void windowClosed(WindowEvent arg0) {
		// TODO Auto-generated method stub
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
	
	private  void setupMenus() {
		// ================ setup menus =================
		JMenu menu;
		JMenuItem menuItem;
		JCheckBoxMenuItem cbMenuItem;
		
		myMenuBar = new JMenuBar();

		//Build the first menu.
		menu = new JMenu("File");
		menu.setMnemonic(KeyEvent.VK_F);
		myMenuBar.add(menu);

		//a group of JMenuItems
		menuItem = new JMenuItem("Quit", KeyEvent.VK_Q);
		menuItem.addActionListener(new ActionListener(){
			@Override	public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Handling  explicit quit menu action");
				if (myQuitter != null) {
					theLogger.info("Setting wantsToQuit flag");
					myQuitter.setWantsToQuit(true);
				}
				setVisible(false);
				dispose();
			}
		});
		menu.add(menuItem);

		//Build second menu in the menu bar.
		menu = new JMenu("Filters");
		menu.setMnemonic(KeyEvent.VK_N);
		
		cbMenuItem = new JCheckBoxMenuItem("Grayscale");
		cbMenuItem.setMnemonic(KeyEvent.VK_G);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  myBackupFilterSeq.addOrReplaceByClass(new Grayscale());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   myBackupFilterSeq.removeByClass(new Grayscale());
			}
		});
		menu.add(cbMenuItem);

		cbMenuItem = new JCheckBoxMenuItem("Blur");
		cbMenuItem.setMnemonic(KeyEvent.VK_B);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  myBackupFilterSeq.addOrReplaceByClass(new Blur());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   myBackupFilterSeq.removeByClass(new Blur());
			}
		});
		menu.add(cbMenuItem);
		
		cbMenuItem = new JCheckBoxMenuItem("FaceDetect");
		cbMenuItem.setMnemonic(KeyEvent.VK_F);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  myBackupFilterSeq.addOrReplaceByClass(new FaceDetector());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   myBackupFilterSeq.removeByClass(new FaceDetector());
			}
		});
		menu.add(cbMenuItem);
		
		cbMenuItem = new JCheckBoxMenuItem("Dilate");
		cbMenuItem.setMnemonic(KeyEvent.VK_D);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  myBackupFilterSeq.addOrReplaceByClass(new Dilate());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   myBackupFilterSeq.removeByClass(new Dilate());
			}
		});
		menu.add(cbMenuItem);
		
		cbMenuItem = new JCheckBoxMenuItem("Erode");
		cbMenuItem.setMnemonic(KeyEvent.VK_E);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  myBackupFilterSeq.addOrReplaceByClass(new Erode());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   myBackupFilterSeq.removeByClass(new Erode());
			}
		});
		menu.add(cbMenuItem);

		cbMenuItem = new JCheckBoxMenuItem("Profile");
		cbMenuItem.setMnemonic(KeyEvent.VK_P);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  myBackupFilterSeq.addOrReplaceByClass(new ProfileDetector());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   myBackupFilterSeq.removeByClass(new ProfileDetector());
			}
		});
		menu.add(cbMenuItem);

		myMenuBar.add(menu);

		setJMenuBar(myMenuBar);
	}
	

	private void registerWindowListeners() { 
		this.addWindowListener(this);

		/* This makes the vision close if we merely click in it
		b.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent arg0) {
				setWantsToQuit(true);
				setVisible(false);
				dispose();
			}
		});
		*/		
	}


	
	
}
