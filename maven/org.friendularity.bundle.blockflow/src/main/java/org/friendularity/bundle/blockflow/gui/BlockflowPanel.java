/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.gui;

import java.awt.Color;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFrame;
import javax.swing.JPanel;
import org.friendularity.bundle.blockflow.engine.BlockflowEngine;
import org.friendularity.bundle.blockflow.util.OSGi_ImageLoader;

/**
 *   The main UI panel
 * 
 *  With the current UI design it's the 'only' panel visually,
 * Though sometimes it actually has children - e.g. if you add a block with a text entry field
 * 
 * @author Annie
 */
class BlockflowPanel extends JPanel implements MouseListener, MouseMotionListener, MouseWheelListener {
	BufferedImage theBackground = null;
	
	BlockflowEngine myEngine;
	
	WindowWidgets myWW;
	
	public BlockflowPanel(BlockflowEngine anEngine)
	{
		super(true);  // make sure it's double buffered
		this.setLayout(null);
		
		myEngine = anEngine;

		try {
			theBackground = OSGi_ImageLoader.getDefaultImageLoader().getImageResource("/img/background.png");
		} catch (IOException ex) {
			Logger.getLogger(BlockflowPanel.class.getName()).log(Level.SEVERE, "CANT FIND BACKGROUND ", ex);
		}
		
		// yup, thats a leeking this - squish squish
		myWW = new WindowWidgets(this);
		
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
		this.addMouseWheelListener(this);
	}

	@Override
	public void paint(Graphics g) {
		super.paint(g); //To change body of generated methods, choose Tools | Templates.
		
		Graphics2D g2 = (Graphics2D)g;
		
		Rectangle r = new Rectangle();
		
		myEngine.getViewport().getBlocksInclusive(g2.getClipBounds(), r);
		Rectangle pos = new Rectangle();

		// paint the background
		for(int i = r.x ; i < r.x + r.width ; i++)
			for(int j = r.y ; j < r.y + r.height ; j++)
			{
				myEngine.getViewport().blockLocation(i, j, pos);
				g2.drawImage(theBackground, pos.x, pos.y, pos.width, pos.height, this);
			}
		
		// paint the window widgets
		myWW.paint(g2);
		
		// paint the border
		g2.setColor(Color.black);
		g2.drawRect(0, 0, this.getWidth() - 1, this.getHeight() - 1);
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mousePressed(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseExited(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseWheelMoved(MouseWheelEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	// TODO memoize this
	private JFrame getFrame() {
		Container c = this;
		
		do
		{
			c = c.getParent();
		} while(c != null && ! (c instanceof JFrame));

		return (JFrame)c;
	}
	/**
	 * Make the window resize so I'm this big
	 * 
	 * @param x
	 * @param y 
	 */
	void beSize(int x, int y) {
		getFrame().setSize(x,y);
	}

	/**
	 * Make the window move so my 0,0 ends up in what used to be local position this.
	 * 
	 * @param x
	 * @param y 
	 */
	void beLocation(int x, int y) {
		Point oldLoc = getFrame().getLocation();
		
		getFrame().setLocation(oldLoc.x + x, oldLoc.y + y);
	}

	void iconize() {
		getFrame().setExtendedState(JFrame.ICONIFIED);
	}

	void maximize() {	
		getFrame().setExtendedState(JFrame.MAXIMIZED_BOTH);
	}

	void close() {			
		getFrame().setVisible(false);
	}

	void deMaximize() {
		getFrame().setExtendedState(JFrame.NORMAL);
	}

	BlockViewport getViewport() {
		return myEngine.getViewport();
	}
}
