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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
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
public class HorBentoSplitter extends BentoSplitter {

	private static final int REALLY_TALL = 32768;
	
	private static BufferedImage texture = null;
	private static BufferedImage textop = null;
	
	public HorBentoSplitter() {
		this.setLayout(null);
		this.setSize(getPreferredSize());
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
	
	
}
