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

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.bento.util.Bento_OSGi_ResourceLoader;

/**
 *   This is a tall skinny splitter, named because it splits things in two horizontally
 * 
 * @author Annie
 */
public class HorBentoSplitter extends BentoSplitter implements MouseListener, MouseMotionListener {

	private static final int REALLY_TALL = 32768;
	
	private static BufferedImage texture = null;
	private static BufferedImage textop = null;
	
	public HorBentoSplitter() {
		super();
		
		this.setLayout(null);
		this.setSize(getPreferredSize());
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
	}

	@Override
	public Dimension getPreferredSize() {
		return new Dimension(MergeGrid.SEPARATOR_WIDTH, REALLY_TALL);
	}

	@Override
	public Dimension getMaximumSize() {
		return getPreferredSize();
	}

	@Override
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	private static final int IMAGE_WIDTH = 10;
	private static final int IMAGE_TOP_HEIGHT = 26;
	private static final int IMAGE_HEIGHT = 168;
	
	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		Graphics2D g2 = ((Graphics2D)g);
		
		if(texture == null)
		{
			try {
				texture = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/horsplittermain.png");
			} catch (IOException ex) {
				Logger.getLogger(HorBentoSplitter.class.getName()).log(Level.SEVERE, null, ex);
				return;
			}
		}
		
		if(textop == null)
		{
			try {
				textop = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/horsplittertop.png");
			} catch (IOException ex) {
				Logger.getLogger(HorBentoSplitter.class.getName()).log(Level.SEVERE, null, ex);
				return;
			}
		}
		
		for(int y = IMAGE_TOP_HEIGHT ; y < g2.getClipBounds().y + g2.getClipBounds().height; y += IMAGE_HEIGHT)
		{
			g2.drawImage(texture, 0, y, this);
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
	protected MergeGrid mg()
	{
		return ((MergeGrid)this.getParent());
	}
	
	@Override
	protected void setMoveCursor() {

		// suboptimal, but the glass pane is only one who gets to actually control cursor
		mg().getGlassPane().setHorMoveCursor();
	}

	protected void setDragCursor() {
		// suboptimal, but the glass pane is only one who gets to actually control cursor
		mg().getGlassPane().setHorDragCursor();
	}

	@Override
	public void mouseClicked(MouseEvent e) {

	}

	@Override
	public void mousePressed(MouseEvent e) {
		setDragCursor();
		
		MergeGrid mg = mg();
		
		if (mg().isLastRowOrColumnSplitter(this))
		{
			throw new IllegalArgumentException("I'm the last column don't try to move me");
		}
		
		prevXOnScreen = e.getXOnScreen();
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
		int delta = e.getXOnScreen() - prevXOnScreen;
		
		int newdelta = mg().resizeColumns(mg().indexOfHorSplitter(this), delta);

		prevXOnScreen = prevXOnScreen + newdelta;
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		setMoveCursor();
	}

	private int prevXOnScreen;
}
