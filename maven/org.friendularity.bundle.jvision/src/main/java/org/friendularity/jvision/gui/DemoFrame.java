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
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;


public class DemoFrame extends JFrame implements WindowListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = -2429127330233038194L;

	private ImageIcon image_button = new ImageIcon();
	private JButton b = new JButton();
	private Boolean quit_flag  = Boolean.FALSE;
	
	private JPanel controls = new JPanel();
	private JLabel framerate = new JLabel();
	private JMenuBar menuBar;
	
	// this is just protection, if we have no other we'll use this one
	FilterSequence filters = new FilterSequence();
	
	public DemoFrame()
	{
		this.setSize(640, 640);
		this.setTitle("OpenCV On Java");
		
		b.setIcon(image_button);
		b.setMinimumSize(new Dimension(640, 480));
		b.setSize(new Dimension(640, 480));
		
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(b, BorderLayout.CENTER);
		b.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent arg0) {
				setWantsToQuit(true);
				setVisible(false);
				dispose();
			}
		});
		
		controls.setMinimumSize(new Dimension(640, 160));
		controls.setBackground(new Color(255, 200, 128));
		controls.add(framerate);
		this.getContentPane().add(controls, BorderLayout.PAGE_END);
		
		
		// ================ setup menus =================
		JMenu menu;
		JMenuItem menuItem;
		JCheckBoxMenuItem cbMenuItem;
		
		menuBar = new JMenuBar();

		//Build the first menu.
		menu = new JMenu("File");
		menu.setMnemonic(KeyEvent.VK_F);
		menuBar.add(menu);

		//a group of JMenuItems
		menuItem = new JMenuItem("Quit",
		                         KeyEvent.VK_Q);
		menuItem.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent arg0) {
				setWantsToQuit(true);
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
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  filters.addOrReplaceByClass(new Grayscale());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   filters.removeByClass(new Grayscale());
			}
		});
		menu.add(cbMenuItem);

		cbMenuItem = new JCheckBoxMenuItem("Blur");
		cbMenuItem.setMnemonic(KeyEvent.VK_B);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  filters.addOrReplaceByClass(new Blur());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   filters.removeByClass(new Blur());
			}
		});
		menu.add(cbMenuItem);
		
		cbMenuItem = new JCheckBoxMenuItem("FaceDetect");
		cbMenuItem.setMnemonic(KeyEvent.VK_F);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  filters.addOrReplaceByClass(new FaceDetector());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   filters.removeByClass(new FaceDetector());
			}
		});
		menu.add(cbMenuItem);
		
		cbMenuItem = new JCheckBoxMenuItem("Dilate");
		cbMenuItem.setMnemonic(KeyEvent.VK_D);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  filters.addOrReplaceByClass(new Dilate());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   filters.removeByClass(new Dilate());
			}
		});
		menu.add(cbMenuItem);
		
		cbMenuItem = new JCheckBoxMenuItem("Erode");
		cbMenuItem.setMnemonic(KeyEvent.VK_E);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  filters.addOrReplaceByClass(new Erode());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   filters.removeByClass(new Erode());
			}
		});
		menu.add(cbMenuItem);

		cbMenuItem = new JCheckBoxMenuItem("Profile");
		cbMenuItem.setMnemonic(KeyEvent.VK_P);
		cbMenuItem.addItemListener(new ItemListener(){
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
				  filters.addOrReplaceByClass(new ProfileDetector());
				else if (e.getStateChange() == ItemEvent.DESELECTED)
				   filters.removeByClass(new ProfileDetector());
			}
		});
		menu.add(cbMenuItem);
		
		menuBar.add(menu);
		
		// =================
		
		setJMenuBar(menuBar);
		
		this.addWindowListener(this);
		
		this.setVisible(true);
		
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		
	}
	
	public boolean wantsToQuit()
	{
		synchronized(quit_flag)
		{
			return quit_flag;
		}
	}
	
	protected void setWantsToQuit(boolean x)
	{
		synchronized(quit_flag)
		{
			quit_flag = new Boolean(x);
		}
		
	}
	
	public void setImage(BufferedImage img){
		image_button.setImage(img);
		this.repaint();
	}
	

	public void setControlledFilterSequence(FilterSequence filters) {
		if(filters == null)throw new IllegalArgumentException("dont set the controlled filters to nothing");
		this.filters = filters;
		
	}


	public void setFramerateMessage(String string) {
		framerate.setText(string);
		
	}
	
	// ========================  WindowListener  ========================
	@Override
	public void windowActivated(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosed(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosing(WindowEvent arg0) {
		setWantsToQuit(true);
		
	}

	@Override
	public void windowDeactivated(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowDeiconified(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowIconified(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowOpened(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}



	
	
}
