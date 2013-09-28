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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import javax.swing.JSplitPane;

import org.friendularity.jvision.engine.Displayer;
import org.friendularity.jvision.engine.JVisionEngine;

/**
 *
 * @author Annie
 */
class CameraViewer extends BentoPlugin  implements Displayer {

	private BufferedImage mImage = null;
	private String mFrameMessage = "";
	private int noImage = 0;
	
	public CameraViewer() {
		super();
		
		this.setPreferredSize(new Dimension(640, 480));
		this.setMinimumSize(new Dimension(160, 120));
		JVisionEngine.getDefaultJVisionEngine().addDisplayer(this);
	}
	

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g); 
		
		if(mImage != null)
		{
			Rectangle destRect = new Rectangle();
			bestAntiAliasedFit(this.getVisibleRect(), mImage.getWidth(), mImage.getHeight(), destRect);
			g.drawImage(mImage, destRect.x, destRect.y, destRect.width, destRect.height, this);
		}
		else if (++noImage % 1000 == 0)
		{
			System.err.println("no image");
		}
		/*
		g.setColor(Color.red);	
		g.drawString("size: " + this.getSize().toString(), 200, 50);
		g.drawString("loc: " + this.getLocation().toString(), 200, 75);
		g.drawString("clip: " + this.getVisibleRect().toString(), 200, 100);
		*/
		// mFrameMessage has a valid message
	}

	@Override
	public void setDisplayedImage(BufferedImage img) {
		mImage = img;
		
		this.repaint();
	}

	@Override
	public void setFramerateMessage(String string) {
		mFrameMessage = string;
	}

	/**
	 *  Lay out an image in the given area so it shows the best location
	 *  The best location is the size of the layout area or smaller, the right aspect ratio, and 
	 *  a small numbered divisor or multiplicand of the original size, and centered in gthe layout area
	 * 
	 * @param layoutArea  rectangle to lay out in
	 * @param width image width
	 * @param height image height
	 * @param destRect   location to draw into
	 */
	private static void bestAntiAliasedFit(Rectangle layoutArea, int width, int height, Rectangle destRect) {
		int divisor = 1;
		
		for( ; divisor < 2 << 16; divisor = divisor << 1)
		{
			if (layoutArea.width >= 256 * width / divisor && layoutArea.height >= 256 * height / divisor)
				break;
		}
		
		destRect.width = 256 * width / divisor;
		destRect.height = 256 * height / divisor;
		
		destRect.x = layoutArea.x + layoutArea.width / 2 - destRect.width / 2;
		destRect.y = layoutArea.y + layoutArea.height / 2 - destRect.height / 2;
	}
}
