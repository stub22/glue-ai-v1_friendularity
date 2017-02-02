/**
 * This is the missing license file
 */
package org.friendularity.bundle.blockflow.gui;

import org.friendularity.bundle.blockflow.engine.BlockflowEngine;
import org.friendularity.bundle.blockflow.engine.BlockflowEngineChangedListener;
import org.friendularity.bundle.blockflow.engine.BlockishThing;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.*;

/**
 * The main UI panel
 *
 * With the current UI design it's the 'only' panel visually,
 * Though sometimes it actually has children - e.g. if you add a block with a text entry field
 *
 * @author Annie
 */
class BlockflowPanel extends JPanel implements
		MouseListener,
		MouseMotionListener,
		MouseWheelListener,
		BlockflowEngineChangedListener {
	private BufferedImage theBackground = null;
	private BufferedImage theBackgroundSmall = null;
	private BufferedImage theBackgroundTiny = null;

	private BufferedImage theBackgroundMarkers = null;

	private BlockflowEngine myEngine;

	private WindowWidgets myWW;

	private BlockViewportController myVPController;
	private ProtoSelectorController myProtoSelectorController;

	private static final int MARKER_WIDTH = 8000;
	public static final int MARKER_HEIGHT = 3724;

	/**
	 * max number of blocks across the viewport before we greek them out
	 */
	private static final int MAX_BKGND_TO_DRAW = 30;

	public BlockflowPanel(BlockflowEngine anEngine) {
		super(true);  // make sure it's double buffered
		this.setLayout(null);

		myEngine = anEngine;
		myEngine.addEngineListener(this);

		try {
			theBackground = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/background.png");
			theBackgroundSmall = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/backgroundsmall.png");
			theBackgroundTiny = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/backgroundtiny.png");
			theBackgroundMarkers = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/backgroundmarkers.png");
		} catch (IOException ex) {
			LoggerFactory.getLogger(BlockflowPanel.class.getName()).error("CANT FIND BACKGROUND ", ex);
		}

		// yup, thats a leeking this - squish squish
		myWW = new WindowWidgets(this);
		myVPController = new BlockViewportController(myEngine);
		myProtoSelectorController = new ProtoSelectorController(myEngine);
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
		this.addMouseWheelListener(this);
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;

		Rectangle r = new Rectangle();

		myEngine.getViewport().getBlocksInclusive(g2.getClipBounds(), r);
		Rectangle pos = new Rectangle();

		// paint the background
		BufferedImage bkgnd;
		if (myEngine.getViewport().getZoom() < 0.5)
			bkgnd = theBackgroundTiny;
		else if (myEngine.getViewport().getZoom() < 1.0)
			bkgnd = theBackgroundSmall;
		else
			bkgnd = theBackground;

		// omit altogether if we're zoomed out too far
		if (r.width < MAX_BKGND_TO_DRAW)
			for (int i = r.x; i < r.x + r.width; i++)
				for (int j = r.y; j < r.y + r.height; j++) {
					myEngine.getViewport().blockLocation(i, j, pos);
					g2.drawImage(bkgnd, pos.x, pos.y, pos.width, pos.height, this);
				}

		// paint the background markers
		myEngine.getViewport().blockLocation(0, 0, pos);
		g2.drawImage(theBackgroundMarkers, pos.x, pos.y,
				(int) (myEngine.getViewport().getZoom() * MARKER_WIDTH),
				(int) (myEngine.getViewport().getZoom() * MARKER_HEIGHT), this);

		// paint the components
		for (int i = r.x; i < r.x + r.width; i++)
			for (int j = r.y; j < r.y + r.height; j++) {
				myEngine.getViewport().blockLocation(i, j, pos);
				BlockishThing thing = myEngine.getModel().getThing(i, j);
				thing.paint(g2, pos, this);
			}

		myProtoSelectorController.paintDecorations(g2);
		// paint the window widgets
		myWW.paint(g2);

		// paint the border
		g2.setColor(Color.black);
		g2.drawRect(0, 0, this.getWidth() - 1, this.getHeight() - 1);
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myProtoSelectorController.mouseClicked(e)) return;
	}

	@Override
	public void mousePressed(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myVPController.mousePressed(e)) return;
		if (myProtoSelectorController.mousePressed(e)) return;
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myVPController.mouseReleased(e)) return;
		if (myProtoSelectorController.mouseReleased(e)) return;
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myProtoSelectorController.mouseEntered(e)) return;
	}

	@Override
	public void mouseExited(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myProtoSelectorController.mouseExited(e)) return;
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myVPController.mouseDragged(e)) return;
		if (myProtoSelectorController.mouseDragged(e)) return;
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myProtoSelectorController.mouseMoved(e)) return;
	}

	@Override
	public void mouseWheelMoved(MouseWheelEvent e) {
		if (myWW.handlesMouseEvent(e)) return;
		if (myVPController.mouseWheelMoved(e)) return;
		if (myProtoSelectorController.mouseWheelMoved(e)) return;
	}

	// TODO memoize this
	private JFrame getFrame() {
		Container c = this;

		do {
			c = c.getParent();
		} while (c != null && !(c instanceof JFrame));

		return (JFrame) c;
	}

	/**
	 * Make the window resize so this panel is this big
	 */
	void beSize(int x, int y) {
		getFrame().setSize(x, y);
	}

	/**
	 * Make the window move so my 0,0 ends up in what used to be local position this.
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
	public void engineChanged(BlockflowEngine engine) {
		repaint(50L);
	}
}
