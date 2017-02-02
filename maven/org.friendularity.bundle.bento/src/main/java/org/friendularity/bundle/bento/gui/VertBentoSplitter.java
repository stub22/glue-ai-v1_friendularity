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
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.image.BufferedImage;
import java.io.IOException;

/**
 * This is the wide, flat splitter used to split things vertically
 *
 * @author Annie
 */
public class VertBentoSplitter extends BentoSplitter implements MouseListener, MouseMotionListener {

	private static final int REALLY_WIDE = 32768;

	private static BufferedImage texture = null;
	private static BufferedImage textop = null;

	public VertBentoSplitter() {
		super();

		this.setLayout(null);
		this.setSize(getPreferredSize());
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
	}

	@Override
	public Dimension getPreferredSize() {
		return new Dimension(REALLY_WIDE, MergeGrid.SEPARATOR_HEIGHT);
	}

	@Override
	public Dimension getMaximumSize() {
		return getPreferredSize();
	}

	@Override
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	private static final int IMAGE_TOP_WIDTH = 26;
	private static final int IMAGE_WIDTH = 168;

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = ((Graphics2D) g);

		if (texture == null) {
			try {
				texture = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/vertsplittermain.png");
			} catch (IOException ex) {
				LoggerFactory.getLogger(VertBentoSplitter.class).error(ex.getMessage(), ex);
				return;
			}
		}

		if (textop == null) {
			try {
				textop = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/vertsplittertop.png");
			} catch (IOException ex) {
				LoggerFactory.getLogger(VertBentoSplitter.class).error(ex.getMessage(), ex);
				return;
			}
		}

		for (int x = IMAGE_TOP_WIDTH; x < g2.getClipBounds().x + g2.getClipBounds().width; x += IMAGE_WIDTH) {
			g2.drawImage(texture, x, 0, this);
		}
		g2.drawImage(textop, 0, 0, this);
	}

	@Override
	protected void paintBorder(Graphics g) {
		// we don't want a border
	}

	/**
	 * Convenience method
	 *
	 * @return my MergeGrid
	 */
	protected MergeGrid mg() {
		return ((MergeGrid) this.getParent());
	}

	@Override
	protected void setMoveCursor() {

		// suboptimal, but the glass pane is only one who gets to actually control cursor
		mg().getGlassPane().setVertMoveCursor();
	}

	protected void setDragCursor() {
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		mg().getGlassPane().setVertDragCursor();
	}

	@Override
	public void mouseClicked(MouseEvent e) {

	}

	@Override
	public void mousePressed(MouseEvent e) {
		setDragCursor();

		MergeGrid mg = mg();

		if (mg().isLastRowOrColumnSplitter(this)) {
			throw new IllegalArgumentException("I'm the last column don't try to move me");
		}

		prevYOnScreen = e.getYOnScreen();
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		setMoveCursor();
	}

	@Override
	public void mouseEntered(MouseEvent e) {

	}

	@Override
	public void mouseExited(MouseEvent e) {

	}

	@Override
	public void mouseDragged(MouseEvent e) {
		setDragCursor();
		int delta = e.getYOnScreen() - prevYOnScreen;

		int newdelta = mg().resizeRows(mg().indexOfVertSplitter(this), delta);

		prevYOnScreen = prevYOnScreen + newdelta;
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		setMoveCursor();
	}

	private int prevYOnScreen;
}
