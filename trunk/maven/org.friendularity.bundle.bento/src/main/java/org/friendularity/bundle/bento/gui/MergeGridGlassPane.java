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
package org.friendularity.bundle.bento.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import org.friendularity.bundle.bento.util.Bento_OSGi_ResourceLoader;



/**
 *
 * @author Annie
 */
class MergeGridGlassPane extends JPanel implements MouseListener, MouseMotionListener {
	private boolean iHandleEvents = false;

	public MergeGridGlassPane() {
		this.setOpaque(false);
		this.setDoubleBuffered(false);
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
	}
	
	@Override
	public boolean isOptimizedDrawingEnabled() {
		return false; // ensures that z order happens properly
	}

	@Override
	protected void paintComponent(Graphics g) {
		Graphics2D g2 = (Graphics2D)g;

		if(isHorSplitDragging)
		{
			g2.setColor(new Color(128, 128,128, 128));
			g2.fillRect(curHorSplitterWidgetX, 0, MergeGrid.SEPARATOR_WIDTH, this.getHeight());
		}
	}

	@Override
	protected void paintBorder(Graphics g) {
		
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		redispatchEvent(e);
	}

	@Override
	public void mousePressed(MouseEvent e) {
		redispatchEvent(e);
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		redispatchEvent(e);
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		redispatchEvent(e);
	}

	@Override
	public void mouseExited(MouseEvent e) {
		redispatchEvent(e);
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		redispatchEvent(e);
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		redispatchEvent(e);
	}

	private boolean handleEventLocally(MouseEvent e)
	{
		MergeGrid mg = ((MergeGrid)this.getParent());
		
		if(e.isControlDown() || 
				iHandleEvents ||
				mg.isLastRowOrColumnSplitter(mg.getNonGlassComponentAt(e.getPoint())))
		{
			switch(e.getID())
			{
				case	MouseEvent.MOUSE_CLICKED: return true;
				case	MouseEvent.MOUSE_DRAGGED:
					localDrag(e);
					return true;
				case	MouseEvent.MOUSE_ENTERED: return true;
				case	MouseEvent.MOUSE_EXITED: return true;
				case	MouseEvent.MOUSE_MOVED:
					localMove(e);
					return true;
				case	MouseEvent.MOUSE_PRESSED:
					localDown(e);
					return true;
				case	MouseEvent.MOUSE_RELEASED:
					localUp(e);
					return true;
				default: return false;
			}
		}
		
		return false;
	}
	
	private Component draggingComponent = null;
	
	private void redispatchEvent(MouseEvent e) {
		if(handleEventLocally(e))
			return;
		
		MergeGrid mg = ((MergeGrid)this.getParent());
		
		Point epoint = SwingUtilities.convertPoint(this, e.getPoint(), mg);
		
		Component target = mg.getNonGlassComponentAt(epoint);
		
		// Make sure we keep tracking same component during drag
		if (e.getID() == MouseEvent.MOUSE_RELEASED)
			draggingComponent = null;
		
		if (draggingComponent != null)
			target = draggingComponent;
		
		if (e.getID() == MouseEvent.MOUSE_PRESSED)
			draggingComponent = target;
		
		if (target == null)
		{
			this.setCursor(Cursor.getDefaultCursor());
			return;
		}
		
		Point tpoint = SwingUtilities.convertPoint(this, e.getPoint(), target);
		
		target.dispatchEvent(new MouseEvent(target,
								e.getID(),
								e.getWhen(),
								e.getModifiers(),
								tpoint.x,
								tpoint.y,
								e.getClickCount(),
								e.isPopupTrigger()));
	}

	private void localDown(MouseEvent e) {
		MergeGrid mg = ((MergeGrid)this.getParent());
		
		Point epoint = SwingUtilities.convertPoint(this, e.getPoint(), mg);
		
		Component target = mg.getNonGlassComponentAt(epoint);
		if (target == null)
			startRCDragSelect(e);
		else if (target instanceof BentoPlugin)
			startRCDragSelect(e);
		else if (target instanceof HorBentoSplitter)
			startHorSplit(e, (HorBentoSplitter)target);
	}

	private void startRCDragSelect(MouseEvent e) {
		// @TODO 
	}

	private int splitCursorFromLeft;
	private int curHorSplitterWidgetX = 0;
	private boolean isHorSplitDragging = false;
	
	private void startHorSplit(MouseEvent e, HorBentoSplitter target) {
 		iHandleEvents = true;
		isHorSplitDragging = true;
		
		splitCursorFromLeft = e.getX() - target.getX();
		curHorSplitterWidgetX = target.getX();
		
		target.setDragCursor();
		this.repaint(50L);
	}

	private void localDrag(MouseEvent e) {
		if(isHorSplitDragging)
		{
			curHorSplitterWidgetX = e.getX() - splitCursorFromLeft;
			this.setHorDragCursor();
			this.repaint(50L);
		}
	}

	private void localMove(MouseEvent e) {
		MergeGrid mg = ((MergeGrid)this.getParent());
		
		Point epoint = SwingUtilities.convertPoint(this, e.getPoint(), mg);
		
		Component target = mg.getNonGlassComponentAt(epoint);
		if (target == null)
			setCursor(Cursor.getDefaultCursor());
		else if (target instanceof BentoPlugin)
			setMergeCursor();
		else if (target instanceof HorBentoSplitter)
			setHorMoveCursor();

		iHandleEvents = false;
		isHorSplitDragging = false;
	}

	private void localUp(MouseEvent e) {
		if(isHorSplitDragging)
		{
			MergeGrid mg = ((MergeGrid)this.getParent());
			try {
				mg.insertColAt(curHorSplitterWidgetX);
				repaint(50L);
			} catch (BadPositionForAddedRowOrColumn ex) {
				Logger.getLogger(MergeGridGlassPane.class.getName()).log(Level.INFO, "Attempt to drop column in invalid location");
			}

			
		}
		
		iHandleEvents = false;
		isHorSplitDragging = false;
	}
	
	private static Cursor sideSideArrow = null;
	private static Cursor activeSideSideArrow = null;
	private static Cursor mergeCursor = null;
	
	void setHorMoveCursor() {
		if(sideSideArrow == null)
			sideSideArrow = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/sidesidearrow.png", 16, 16, "sidesidearrow");
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid)this.getParent()).getGlassPane().setCursor(sideSideArrow);
	}

	void setHorDragCursor() {
		if(activeSideSideArrow == null)
			activeSideSideArrow = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/activesidesidearrow.png", 16, 16, "activesidesidearrow");
		
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid)this.getParent()).getGlassPane().setCursor(activeSideSideArrow);
	}

	private void setMergeCursor() {
		if(mergeCursor == null)
			mergeCursor = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/mergecursor.png", 16, 16, "mergecursor");
		
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid)this.getParent()).getGlassPane().setCursor(mergeCursor);
	}
}
