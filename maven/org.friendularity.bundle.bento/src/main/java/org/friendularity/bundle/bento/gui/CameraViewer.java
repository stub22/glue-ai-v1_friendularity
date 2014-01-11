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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.bento.util.Bento_OSGi_ResourceLoader;
import org.friendularity.jvision.broker.ImageFlavorNotAvailable;
import org.friendularity.jvision.broker.ImageStreamBroker;

import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.broker.ImageStreamImage;

/**
 *
 * @author Annie
 */
public class CameraViewer extends BentoPlugin  implements ImageStreamConsumer {

	private BufferedImage mImage = null;
	private String mFrameMessage = "";
	private int noImage = 0;
	
	private static int nextImageNum = 1;
	
	private int thisImageNum;
	private static Font cameraViewerFont = new Font(Font.SERIF, Font.PLAIN, 18);
	
	public CameraViewer(String name) {
		super();
		thisImageNum = nextImageNum++;
		
		this.setPreferredSize(new Dimension(640, 480));
		this.setMaximumSize(new Dimension(640, 480));
		this.setMinimumSize(new Dimension(160, 120));
		this.setLayout(null);
		this.setSize(new Dimension(640, 480));

		ImageStreamBroker.getDefaultImageStreamBroker().alwaysAddImageStreamConsumer(
				name, this);
	}
	

	@Override
	protected void paintComponent(Graphics g) {
		// super.paintComponent(g); 
		
		g.setColor(Color.DARK_GRAY);
		Rectangle vis = this.getVisibleRect();
		g.fillRect(vis.x, vis.y, vis.width, vis.height);
		g.setFont(cameraViewerFont);
		if(mImage != null)
		{
			Rectangle destRect = new Rectangle();
			bestAntiAliasedFit(this.getVisibleRect(), mImage.getWidth(), mImage.getHeight(), destRect);
			g.drawImage(mImage, destRect.x, destRect.y, destRect.width, destRect.height, this);
			drawLabel(g, this.getHeight() - 30,
					"(" + Integer.toString(thisImageNum) +
					") " + mFrameMessage);
		}
		else 
		{
			drawLabel(g, this.getHeight() - 30,
					"no image " + mFrameMessage);
		}
	}
	

	@Override
	public void setConsumedImage(ImageStreamImage img) {
		try {
			mImage = img.getBufferedImage();
		} catch (ImageFlavorNotAvailable ex) {
			Logger.getLogger(CameraViewer.class.getName()).log(Level.SEVERE, "CameraViewer viewing wrong image type");
		}
		
		this.repaint();
	}

	@Override
	public void setConsumedMessage(String string) {
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

	@Override
	public void sourceIsEnding() {
		try {
			mImage = Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/testpattern.png");
		} catch (IOException ex) {
			Logger.getLogger(CameraViewer.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	private void drawLabel(Graphics g, int vertPos, String s) {
		g.setColor(Color.WHITE);
		g.setFont(cameraViewerFont);
		int x = this.getWidth() / 2 - g.getFontMetrics().stringWidth(s) / 2;
		g.drawString(s, x -1 , vertPos - 1);
		g.drawString(s, x + 1 , vertPos + 1);
		g.drawString(s, x -1 , vertPos + 1);
		g.drawString(s, x + 1 , vertPos - 1);
		g.setColor(Color.black);
		g.drawString(s, x , vertPos);
	}
}
