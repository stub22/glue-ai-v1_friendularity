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
 *
 * @author Annie
 */
public class VertBentoSplitter extends BentoSplitter {
	
	private static final int REALLY_WIDE = 32768;
	
	private static BufferedImage texture = null;
	private static BufferedImage textop = null;
	
	public VertBentoSplitter() {
		this.setLayout(null);
		this.setSize(getPreferredSize());
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

	private static final int IMAGE_HEIGHT = 10;
	private static final int IMAGE_LEFT_WIDTH = 26;
	private static final int IMAGE_WIDTH = 168;
	
	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		Graphics2D g2 = ((Graphics2D)g);
		
		if(texture == null)
		{
			try {
				texture = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/vertsplittermain.png");
			} catch (IOException ex) {
				Logger.getLogger(HorBentoSplitter.class.getName()).log(Level.SEVERE, null, ex);
				return;
			}
		}
		
		if(textop == null)
		{
			try {
				textop = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/vertsplittertop.png");
			} catch (IOException ex) {
				Logger.getLogger(HorBentoSplitter.class.getName()).log(Level.SEVERE, null, ex);
				return;
			}
		}
		
		for(int x = IMAGE_LEFT_WIDTH ; x < g2.getClipBounds().x + g2.getClipBounds().width; x += IMAGE_WIDTH)
		{
			g2.drawImage(texture, x, 0, this);
		}
		g2.drawImage(textop, 0, 0, this);
	}

	@Override
	protected void paintBorder(Graphics g) {
		// we don't want a border
	}
	
	
}
