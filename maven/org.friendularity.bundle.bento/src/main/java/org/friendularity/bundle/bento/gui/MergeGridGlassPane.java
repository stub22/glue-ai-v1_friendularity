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

import org.friendularity.bundle.bento.util.Bento_OSGi_ResourceLoader;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Iterator;

import javax.swing.*;

/**
 * @author Annie
 */
class MergeGridGlassPane extends JPanel implements MouseListener, MouseMotionListener {
	private boolean iHandleEvents = false;
	private int newComponentCol;
	private int newComponentRow;

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
		Graphics2D g2 = (Graphics2D) g;

		if (isHorSplitDragging) {
			g2.setColor(new Color(128, 128, 128, 128));
			g2.fillRect(curHorSplitterWidgetX, 0, MergeGrid.SEPARATOR_WIDTH, this.getHeight());
		}
		if (isVertSplitDragging) {
			g2.setColor(new Color(128, 128, 128, 128));
			g2.fillRect(0, curVertSplitterWidgetY, this.getWidth(), MergeGrid.SEPARATOR_HEIGHT);
		}
		
/*
		g2.setColor(Color.red);
		String[] s = debugStr.split(":");
		int yy = this.getHeight() / 2;
		for(int i = 0; i <s.length ; i++)
		{
			g2.drawString(s[i], this.getWidth() / 2, yy);
			yy += 16;
		}
		*/
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

	private boolean handleEventLocally(MouseEvent e) {

		MergeGrid mg = ((MergeGrid) this.getParent());

		if (e.isPopupTrigger()) {
			Component cmpnt = mg.getNonGlassComponentAt(e.getPoint());
			newComponentCol = mg.getColumnAt(e.getPoint().x);
			newComponentRow = mg.getRowAt(e.getPoint().y);

			initPopup(cmpnt,
					newComponentCol,
					newComponentRow); // reinit to get rid of old refs
			popup.show(e.getComponent(),
					e.getX(), e.getY());

			// it's on mouseup
			draggingComponent = null;
			iHandleEvents = false;
			isHorSplitDragging = false;
			isVertSplitDragging = false;
			return true;
		}

		if (e.isControlDown() ||
				iHandleEvents ||
				mg.isLastRowOrColumnSplitter(mg.getNonGlassComponentAt(e.getPoint()))) {
			switch (e.getID()) {
				case MouseEvent.MOUSE_CLICKED:
					return true;
				case MouseEvent.MOUSE_DRAGGED:
					localDrag(e);
					return true;
				case MouseEvent.MOUSE_ENTERED:
					return true;
				case MouseEvent.MOUSE_EXITED:
					return true;
				case MouseEvent.MOUSE_MOVED:
					localMove(e);
					return true;
				case MouseEvent.MOUSE_PRESSED:
					localDown(e);
					return true;
				case MouseEvent.MOUSE_RELEASED:
					localUp(e);
					return true;
				default:
					return false;
			}
		}

		return false;
	}

	private Component draggingComponent = null;

	private void redispatchEvent(MouseEvent e) {
		if (debugEvent(e, handleEventLocally(e)))
			return;

		MergeGrid mg = ((MergeGrid) this.getParent());

		Point epoint = SwingUtilities.convertPoint(this, e.getPoint(), mg);

		Component target = mg.getNonGlassComponentAt(epoint);

		// Make sure we keep tracking same component during drag
		if (e.getID() == MouseEvent.MOUSE_RELEASED)
			draggingComponent = null;

		if (draggingComponent != null)
			target = draggingComponent;

		if (e.getID() == MouseEvent.MOUSE_PRESSED)
			draggingComponent = target;

		if (target == null) {
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
		MergeGrid mg = ((MergeGrid) this.getParent());

		Point epoint = SwingUtilities.convertPoint(this, e.getPoint(), mg);

		Component target = mg.getNonGlassComponentAt(epoint);
		if (target == null)
			startRCDragSelect(e);
		else if (target instanceof BentoPlugin)
			startRCDragSelect(e);
		else if (target instanceof HorBentoSplitter)
			startHorSplit(e, (HorBentoSplitter) target);
		else if (target instanceof VertBentoSplitter)
			startVertSplit(e, (VertBentoSplitter) target);
	}

	private void startRCDragSelect(MouseEvent e) {
		// @TODO 
	}

	private int splitCursorFromLeft;
	private int splitCursorFromTop;
	private int curHorSplitterWidgetX = 0;
	private int curVertSplitterWidgetY = 0;
	private boolean isHorSplitDragging = false;
	private boolean isVertSplitDragging = false;

	private void startHorSplit(MouseEvent e, HorBentoSplitter target) {
		iHandleEvents = true;
		isHorSplitDragging = true;

		splitCursorFromLeft = e.getX() - target.getX();
		curHorSplitterWidgetX = target.getX();

		target.setDragCursor();
		this.repaint(50L);
	}

	private void startVertSplit(MouseEvent e, VertBentoSplitter target) {
		iHandleEvents = true;
		isVertSplitDragging = true;

		splitCursorFromTop = e.getY() - target.getY();
		curVertSplitterWidgetY = target.getY();

		target.setDragCursor();
		this.repaint(50L);
	}

	private void localDrag(MouseEvent e) {
		if (isHorSplitDragging) {
			curHorSplitterWidgetX = e.getX() - splitCursorFromLeft;
			this.setHorDragCursor();
			this.repaint(50L);
		}
		if (isVertSplitDragging) {
			curVertSplitterWidgetY = e.getY() - splitCursorFromTop;
			this.setVertDragCursor();
			this.repaint(50L);
		}
	}

	private void localMove(MouseEvent e) {
		MergeGrid mg = ((MergeGrid) this.getParent());

		Point epoint = SwingUtilities.convertPoint(this, e.getPoint(), mg);

		Component target = mg.getNonGlassComponentAt(epoint);
		if (target == null)
			setCursor(Cursor.getDefaultCursor());
		else if (target instanceof BentoPlugin)
			setMergeCursor();
		else if (target instanceof HorBentoSplitter)
			setHorMoveCursor();
		else if (target instanceof VertBentoSplitter)
			setVertMoveCursor();

		iHandleEvents = false;
		isHorSplitDragging = false;
		isVertSplitDragging = false;
	}

	private void localUp(MouseEvent e) {
		if (isHorSplitDragging) {
			MergeGrid mg = ((MergeGrid) this.getParent());
			try {
				mg.insertColAt(curHorSplitterWidgetX);
				repaint(50L);
			} catch (BadPositionForAddedRowOrColumn ex) {
				LoggerFactory.getLogger(MergeGridGlassPane.class.getName()).info("Attempt to drop column in invalid location");
			}
		}

		if (isVertSplitDragging) {
			MergeGrid mg = ((MergeGrid) this.getParent());
			try {
				mg.insertRowAt(curVertSplitterWidgetY);
				repaint(50L);
			} catch (BadPositionForAddedRowOrColumn ex) {
				LoggerFactory.getLogger(MergeGridGlassPane.class.getName()).info("Attempt to drop row in invalid location");
			}
		}

		iHandleEvents = false;
		isHorSplitDragging = false;
		isVertSplitDragging = false;
	}

	private static Cursor sideSideArrow = null;
	private static Cursor activeSideSideArrow = null;
	private static Cursor updownArrow = null;
	private static Cursor activeUpdownArrow = null;
	private static Cursor mergeCursor = null;

	void setHorMoveCursor() {
		if (sideSideArrow == null)
			sideSideArrow = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/sidesidearrow.png", 16, 16, "sidesidearrow");
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid) this.getParent()).getGlassPane().setCursor(sideSideArrow);
	}

	void setHorDragCursor() {
		if (activeSideSideArrow == null)
			activeSideSideArrow = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/activesidesidearrow.png", 16, 16, "activesidesidearrow");

		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid) this.getParent()).getGlassPane().setCursor(activeSideSideArrow);
	}


	void setVertMoveCursor() {
		if (updownArrow == null)
			updownArrow = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/updownarrow.png", 16, 16, "updownarrow");
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid) this.getParent()).getGlassPane().setCursor(updownArrow);
	}

	void setVertDragCursor() {
		if (activeUpdownArrow == null)
			activeUpdownArrow = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/activeupdownarrow.png", 16, 16, "activeupdownarrow");
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid) this.getParent()).getGlassPane().setCursor(activeUpdownArrow);
	}

	private void setMergeCursor() {
		if (mergeCursor == null)
			mergeCursor = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
					"/img/mergecursor.png", 16, 16, "mergecursor");

		// suboptimal, but the glass pane is only one who gets to actually control cursor
		((MergeGrid) this.getParent()).getGlassPane().setCursor(mergeCursor);
	}

	//===============  Menu handling ========================================


	// @TODO move menu to BentoAction
	public static final String HTWO_MENU = "H Two";
	public static final String HTHREE_MENU = "H Three";
	public static final String HFOUR_MENU = "H Four";
	public static final String VTWO_MENU = "V Two";
	public static final String VTHREE_MENU = "V Three";
	public static final String VFOUR_MENU = "V Four";
	public static final String REMOVE_MENU = "Remove";
	public static final String DELETE_COLUMN = "Delete Column";
	public static final String DELETE_ROW = "Delete Row";

	private boolean canSplit() {
		return true;
	}


	protected void initPopup(final Component c, final int col, final int row) {
		popup = new JPopupMenu();


		JMenuItem menuItem;
		JMenu subMenu;
		
			/*			
		try {		
		// @TODO if you reenable this
		// change all to anonymous ActionListener handlers, we don't want to get the actions
		// for things the plugin adds
		* 
			subMenu = new JMenu("Hor. Split");
			ImageIcon ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/twohor.png"));
			menuItem = new JMenuItem(HTWO_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/threehor.png"));
			menuItem = new JMenuItem(HTHREE_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/fourhor.png"));
			menuItem = new JMenuItem(HFOUR_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			popup.add(subMenu);

			subMenu = new JMenu("Vert. Split");
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/twovert.png"));
			menuItem = new JMenuItem(VTWO_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/threevert.png"));
			menuItem = new JMenuItem(VTHREE_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/fourvert.png"));
			menuItem = new JMenuItem(VFOUR_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			popup.add(subMenu);

		} catch (IOException ex) {
			LoggerFactory.getLogger(MergeGridGlassPane.class).error(ex.getMessage(), ex);
		}	
		*/
		if (c == null || !(c instanceof BentoSplitter)) {
			popup.add(new JSeparator());

			if (((MergeGrid) (MergeGridGlassPane.this.getParent())).getNumColumns() > 1) {
				menuItem = new JMenuItem(DELETE_COLUMN);
				menuItem.addActionListener(new ActionListener() {

					@Override
					public void actionPerformed(ActionEvent e) {
						MergeGridGlassPane.this.draggingComponent = null;
						((MergeGrid) (MergeGridGlassPane.this.getParent())).attemptDeleteColumn(col);
					}
				});
				popup.add(menuItem);
			}

			if (((MergeGrid) (MergeGridGlassPane.this.getParent())).getNumRows() > 1) {
				menuItem = new JMenuItem(DELETE_ROW);
				menuItem.addActionListener(new ActionListener() {

					@Override
					public void actionPerformed(ActionEvent e) {
						MergeGridGlassPane.this.draggingComponent = null;
						((MergeGrid) (MergeGridGlassPane.this.getParent())).attemptDeleteRow(row);
					}
				});
				popup.add(menuItem);
			}
		}

		if (c instanceof BentoPlugin) {
			popup.add(new JSeparator());

			menuItem = new JMenuItem(REMOVE_MENU);
			menuItem.addActionListener(new ActionListener() {

				@Override
				public void actionPerformed(ActionEvent e) {
					draggingComponent = null;
					((BentoPlugin) c).removeFromMergeGrid();
				}
			});

			popup.add(menuItem);
		}

		if (c == null) {
			popup.add(new JSeparator());

			subMenu = new JMenu("Add Item");

			for (Iterator<String> i = ImageStreamBroker.getDefaultImageStreamBroker().imageStreamNames();
				 i.hasNext(); ) {
				String name = i.next();
				menuItem = new JMenuItem(name);
				menuItem.addActionListener(new ActionListener() {

					@Override
					public void actionPerformed(ActionEvent e) {
						MergeGridGlassPane.this.addElementAction(e);
					}
				});

				subMenu.add(menuItem);
			}
			popup.add(subMenu);
		}

		if (c instanceof BentoPlugin) {
			((BentoPlugin) c).addAdditionalMenuItems(popup);
		}
	}

	private JPopupMenu popup;

	/**
	 * get the right click menu
	 *
	 * If you modify it, do it on swingworker thread
	 *
	 * @return popup menu
	 */
	protected JPopupMenu getPopup() {
		return popup;
	}

	private String debugStr = "";

	/**
	 * print
	 */
	private boolean debugEvent(MouseEvent e, boolean handledLocally) {
		debugStr = e.toString() + ":";
		if (iHandleEvents) debugStr += "iHandleEvents:";
		if (isHorSplitDragging) debugStr += "isHorSplitDragging:";
		if (isVertSplitDragging) debugStr += "isVertSplitDragging:";
		if (draggingComponent != null) debugStr += draggingComponent.getClass().getSimpleName();

		this.repaint(50L);
		return handledLocally;
	}

	private void addElementAction(ActionEvent e) {
		MergeGrid mg = ((MergeGrid) (MergeGridGlassPane.this.getParent()));

		CameraViewer cv = new CameraViewer(e.getActionCommand());

		try {
			mg.setCell(cv, newComponentCol, newComponentRow, 1, 1);
		} catch (ItsBentoBoxesNotBentoTetrisException ex) {
			LoggerFactory.getLogger(BentoFrame.class).error(ex.getMessage(), ex);
		}
	}

}
