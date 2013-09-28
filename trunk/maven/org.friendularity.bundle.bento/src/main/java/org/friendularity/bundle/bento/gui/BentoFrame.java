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

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.HeadlessException;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 *
 * @author Annie
 */
public class BentoFrame extends JFrame {

	private static final String DEFAULT_TITLE = "Bento";
	
	public BentoFrame() throws HeadlessException {
		super();
		initialize(DEFAULT_TITLE);
	}

	public BentoFrame(GraphicsConfiguration gc) {
		super(gc);
		initialize(DEFAULT_TITLE);
	}

	public BentoFrame(String title) throws HeadlessException {
		super(title);
		initialize(title);
	}

	public BentoFrame(String title, GraphicsConfiguration gc) {
		super(title, gc);
		initialize(title);
	}

	private void initialize(String title) {
		this.setTitle(title);
		
		this.getContentPane().add(
				new BentoBox(new CameraViewer()), 
				BorderLayout.CENTER);
		
		this.pack();
		this.setVisible(true);
	}
}
