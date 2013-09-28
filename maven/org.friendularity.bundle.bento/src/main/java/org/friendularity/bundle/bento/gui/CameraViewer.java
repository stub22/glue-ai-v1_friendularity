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
import java.awt.image.BufferedImage;

import org.friendularity.jvision.engine.Displayer;
import org.friendularity.jvision.engine.JVisionEngine;

/**
 *
 * @author Annie
 */
class CameraViewer extends BentoPlugin  implements Displayer {

	private BufferedImage mImage = null;
	private String mFrameMessage = "";
	
	public CameraViewer() {
		this.setPreferredSize(new Dimension(640, 480));
	//	JVisionEngine.getDefaultJVisionEngine().addDisplayer(this);
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g); 
		
	//	g.drawImage(mImage, 0, 0, this);
		g.setColor(Color.red);
		g.drawLine(0, 0, 100, 100);
		
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
}
