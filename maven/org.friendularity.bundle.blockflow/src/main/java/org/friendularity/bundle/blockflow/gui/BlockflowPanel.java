/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.gui;

import org.friendularity.bundle.blockflow.engine.BlockishThing;
import org.friendularity.bundle.blockflow.engine.BlockModelChangedListener;
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
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;

/**
 *   The main UI panel
 * 
 *  With the current UI design it's the 'only' panel visually,
 * Though sometimes it actually has children - e.g. if you add a block with a text entry field
 * 
 * @author Annie
 */
class BlockflowPanel extends JPanel implements 
		MouseListener, 
		MouseMotionListener, 
		MouseWheelListener,
		BlockModelChangedListener {
	private BufferedImage theBackground = null;
	private BufferedImage theBackgroundSmall = null;
	private BufferedImage theBackgroundTiny = null;
	
	private BufferedImage theBackgroundMarkers = null;
	
	private BlockflowEngine myEngine;
	
	private WindowWidgets myWW;
	
	private BlockViewportController myVPController;
	
	private static final int MARKER_WIDTH = 8000;
	public static final int MARKER_HEIGHT = 3724;
	
	/**
	 * max number of blocks across the viewport before we greek them out
	 * 
	 */
	private static final int MAX_BKGND_TO_DRAW = 30;
	
	public BlockflowPanel(BlockflowEngine anEngine)
	{
		super(true);  // make sure it's double buffered
		this.setLayout(null);
		
		myEngine = anEngine;
		myEngine.addView(this);

		try {
			theBackground = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/background.png");
			theBackgroundSmall = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/backgroundsmall.png");
			theBackgroundTiny = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/backgroundtiny.png");
			theBackgroundMarkers = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/backgroundmarkers.png");
		} catch (IOException ex) {
			Logger.getLogger(BlockflowPanel.class.getName()).log(Level.SEVERE, "CANT FIND BACKGROUND ", ex);
		}
		
		// yup, thats a leeking this - squish squish
		myWW = new WindowWidgets(this);
		myVPController = new BlockViewportController(myEngine);
				
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
		this.addMouseWheelListener(this);
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g); 
		
		Graphics2D g2 = (Graphics2D)g;
		
		Rectangle r = new Rectangle();
		
		myEngine.getViewport().getBlocksInclusive(g2.getClipBounds(), r);
		Rectangle pos = new Rectangle();

		// paint the background
		BufferedImage bkgnd;
		if(myEngine.getViewport().getZoom() < 0.5)
			bkgnd = theBackgroundTiny;
		else if (myEngine.getViewport().getZoom() < 1.0)
			bkgnd = theBackgroundSmall;
		else
			bkgnd = theBackground;
		
		// omit altogether if we're zoomed out too far
		if(r.width < MAX_BKGND_TO_DRAW)
			for(int i = r.x ; i < r.x + r.width ; i++)
				for(int j = r.y ; j < r.y + r.height ; j++)
				{
					myEngine.getViewport().blockLocation(i, j, pos);
					g2.drawImage(bkgnd, pos.x, pos.y, pos.width, pos.height, this);
				}
		
		// paint the background markers
		myEngine.getViewport().blockLocation(0, 0, pos);
		g2.drawImage(theBackgroundMarkers, pos.x, pos.y, 
				(int)(myEngine.getViewport().getZoom() * MARKER_WIDTH), 
				(int)(myEngine.getViewport().getZoom() * MARKER_HEIGHT), this);
		
		// paint the components
		for(int i = r.x ; i < r.x + r.width ; i++)
			for(int j = r.y ; j < r.y + r.height ; j++)
			{
				myEngine.getViewport().blockLocation(i, j, pos);
				BlockishThing thing = myEngine.getModel().getThing(i, j);
				thing.paint(g2, pos, this);
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
		if(myVPController.mousePressed(e))return;
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
		if(myVPController.mouseReleased(e))return;
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
		if(myVPController.mouseDragged(e))return;
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		if(myWW.handlesMouseEvent(e))return;
	}

	@Override
	public void mouseWheelMoved(MouseWheelEvent e) {
		if(myWW.handlesMouseEvent(e))return;
		if(myVPController.mouseWheelMoved(e))return;
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
	 * Make the window resize so this panel is this big
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

	void close() {			
		getFrame().setVisible(false);
	}

	BlockViewport getViewport() {
		return myEngine.getViewport();
	}

	/**
	 * Set the window state - eg if its maximized, iconified, etd
	 * 
	 * @param state the window extended state, as defined in JFrame
	 */
	void setState(int state) {
		getFrame().setExtendedState(state);
	}

	@Override
	public void modelChanged(BlockflowEngine engine) {
		repaint(50L);
	}
}
