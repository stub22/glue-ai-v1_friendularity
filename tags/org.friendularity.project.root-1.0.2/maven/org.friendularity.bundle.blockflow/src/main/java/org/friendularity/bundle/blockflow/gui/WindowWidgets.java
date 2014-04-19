 /*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.bundle.blockflow.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFrame;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;

/**
 * decorator that handles the more funnified drag bar
 * and resizers. Usually dealing with this stuff is painful
 * in swing anyway, so why not have it exactly like we want?
 * 
 * @author Annie
 *
 */
class WindowWidgets implements ComponentListener {
	private static final int DRAG_HEIGHT = 22;
	private static final int RIGHT_ICONS_WIDTH = 107;
	private static final int RESIZER_HEIGHT = 22;
	private static final int RESIZER_WIDTH = 22;
	private static final long REPAINT_MAX_TIME = 100L;
	private static final int ICONIZE_RIGHT_EDGE = 29;
	private static final int MAXIMIZE_RIGHT_EDGE = 58;
	
	private BufferedImage dragBar;
	private BufferedImage dragBarRightIcons;
	private BufferedImage leftResizer;
	private BufferedImage rightResizer;
	
	private boolean isLeftResizing = false;
	private boolean isRightResizing = false;
	private boolean isDragging = false;
	private boolean isMaximized = false;
	
	private BlockflowPanel bfp;
	
	private Point dragPoint = new Point();
	// because of invalidate time, we may not be the size we've asked for
	// easier to keep track of what size we hope we are
	private Dimension commanded_size = new Dimension(0,0);
	
	WindowWidgets(BlockflowPanel abfp)
	{
		bfp = abfp;
		bfp.addComponentListener(this);
		
		try {
			dragBar = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/dragbar.png");
			dragBarRightIcons = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/righticons.png");
			leftResizer = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/leftresizer.png");
			rightResizer = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/rightresizer.png");
		} catch (IOException ex) {
			Logger.getLogger(WindowWidgets.class.getName()).log(Level.SEVERE, null, ex);
		}
	}
	
	private void repaintWidgets()
	{
		bfp.repaint(REPAINT_MAX_TIME, 0, 0, bfp.getWidth(), DRAG_HEIGHT);	
		bfp.repaint(REPAINT_MAX_TIME, 0, bfp.getHeight() - RESIZER_HEIGHT, RESIZER_HEIGHT, RESIZER_WIDTH);
		bfp.repaint(REPAINT_MAX_TIME, bfp.getWidth() - RESIZER_WIDTH, bfp.getHeight() - RESIZER_HEIGHT, 
				RESIZER_HEIGHT, RESIZER_WIDTH);
	}

	@Override
	public void componentResized(ComponentEvent e) {
		repaintWidgets();
	}

	@Override
	public void componentMoved(ComponentEvent e) {
		;
	}

	@Override
	public void componentShown(ComponentEvent e) {
		repaintWidgets();
	}

	@Override
	public void componentHidden(ComponentEvent e) {
		;
	}

	void paint(Graphics2D g2) {
		g2.setColor(Color.white);
		g2.drawRect(0, 0, bfp.getWidth(), DRAG_HEIGHT);
		
		g2.drawImage(dragBar, 0, 0, bfp);
		g2.drawImage(dragBarRightIcons, bfp.getWidth() - RIGHT_ICONS_WIDTH, 0, bfp);
		g2.drawImage(leftResizer, 0, bfp.getHeight() - RESIZER_HEIGHT, bfp);
		g2.drawImage(rightResizer, bfp.getWidth() - RESIZER_WIDTH, bfp.getHeight() - RESIZER_HEIGHT, bfp);
		g2.setColor(Color.black);
		g2.drawRect(0,0, bfp.getWidth() - 1, DRAG_HEIGHT);
	}

	boolean handlesMouseEvent(MouseEvent e) {
		if(isLeftResizing)
			return handleLeftResize(e);
		else if(isRightResizing)
			return handleRightResize(e);
		else if(isDragging)
			return handleDragging(e);
		else if(e.getID() == MouseEvent.MOUSE_CLICKED &&
				e.getX() > bfp.getWidth() - RIGHT_ICONS_WIDTH &&
				e.getY() < DRAG_HEIGHT)
			return handleRightIconMouse(e);
		else if (e.getID() ==MouseEvent.MOUSE_PRESSED &&
				e.getY() < DRAG_HEIGHT)
		{
			dragPoint.x = e.getX() + bfp.getLocationOnScreen().x;
			dragPoint.y = e.getY() + bfp.getLocationOnScreen().y;
			commanded_size = bfp.getSize();
			isDragging = true;
			return true;			
		}
		else if (e.getID() == MouseEvent.MOUSE_PRESSED &&
				e.getX() < RESIZER_WIDTH &&
				e.getY() > bfp.getHeight() - RESIZER_HEIGHT)
		{
			dragPoint.x = e.getX() + bfp.getLocationOnScreen().x;
			dragPoint.y = e.getY() + bfp.getLocationOnScreen().y;
			commanded_size = bfp.getSize();
			isLeftResizing = true;
			return true;
		}
		else if (e.getID() == MouseEvent.MOUSE_PRESSED &&
				e.getX() > bfp.getWidth() - RESIZER_WIDTH &&
				e.getY() > bfp.getHeight() - RESIZER_HEIGHT)
		{
			dragPoint.x = e.getX() + bfp.getLocationOnScreen().x;
			dragPoint.y = e.getY() + bfp.getLocationOnScreen().y;
			commanded_size = bfp.getSize();
			isRightResizing = true;
			return true;
		}
		else
			return false;
	}

	private boolean handleLeftResize(MouseEvent e) {
		
		if(e.getID() == MouseEvent.MOUSE_RELEASED)
		{
			isLeftResizing = false;
			return true;
		} else if (e.getID() == MouseEvent.MOUSE_DRAGGED)
		{
			int dx = e.getX() + bfp.getLocationOnScreen().x - dragPoint.x;
			int dy = e.getY() +  + bfp.getLocationOnScreen().y - dragPoint.y;
			
			bfp.beLocation(dx, 0);

			commanded_size = new Dimension(commanded_size.width - dx, commanded_size.height + dy);
			bfp.beSize(commanded_size.width, commanded_size.height);
			
			bfp.getViewport().offsetPixels(dx, 0);
			
			// we now just keep the dragPoint under the last cursor pos in global
			dragPoint.x += dx;
			dragPoint.y += dy;
		    return true;
		}
		else	
			return false;
	}

	private boolean handleRightResize(MouseEvent e) {
		if(e.getID() == MouseEvent.MOUSE_RELEASED)
		{
			isRightResizing = false;
			return true;
		} else if (e.getID() == MouseEvent.MOUSE_DRAGGED)
		{
			int dx = e.getX() + bfp.getLocationOnScreen().x - dragPoint.x;
			int dy = e.getY() + bfp.getLocationOnScreen().y - dragPoint.y;
			
			commanded_size = new Dimension(commanded_size.width + dx, commanded_size.height + dy);
			bfp.beSize(commanded_size.width, commanded_size.height);
			
			// we now just keep the dragPoint under the last cursor pos in global
			dragPoint.x += dx;
			dragPoint.y += dy;
		    return true;
		}
		else	
			return false;		
	}

	private boolean handleRightIconMouse(MouseEvent e) {
		int from_left_of_icons = e.getX() - bfp.getWidth() + RIGHT_ICONS_WIDTH;
		
		if( from_left_of_icons < ICONIZE_RIGHT_EDGE)
			bfp.setState(JFrame.ICONIFIED);
		else if (from_left_of_icons < MAXIMIZE_RIGHT_EDGE && isMaximized)
		{
			bfp.setState(JFrame.NORMAL);
			isMaximized = false;
		}
		else if (from_left_of_icons < MAXIMIZE_RIGHT_EDGE)
		{
			bfp.setState(JFrame.MAXIMIZED_BOTH);
			isMaximized = true;
		}
		else
			bfp.close();
		
		return true;
	}

	private boolean handleDragging(MouseEvent e) {
		if(e.getID() == MouseEvent.MOUSE_RELEASED)
		{
			isDragging = false;
			return true;
		} else if (e.getID() == MouseEvent.MOUSE_DRAGGED)
		{
			int dx = e.getLocationOnScreen().x - dragPoint.x;
			int dy = e.getLocationOnScreen().y - dragPoint.y;
			
			bfp.beLocation(dx, dy);
			
			// we now just keep the dragPoint under the last cursor pos in global
			dragPoint.x += dx;
			dragPoint.y += dy;
		    return true;
		}
		else	
			return false;
	}
}
