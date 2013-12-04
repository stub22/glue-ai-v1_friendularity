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
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import org.friendularity.jvision.engine.JVisionEngine;

/**
 *
 * @author Annie
 */
public class BentoFrame extends JFrame {

	private static final String DEFAULT_TITLE = "Bento";

	private static int unusedTitleInt = 0;
	
	private static String getUnusedTitle() {
		if(unusedTitleInt == 0)
		{
			unusedTitleInt = 1;
			return DEFAULT_TITLE;
		}
		else
		{
			return DEFAULT_TITLE + (unusedTitleInt++);
		}
	}
	
	private MergeGrid myGrid;
	
	public BentoFrame() throws HeadlessException {
		super();
		initialize(getUnusedTitle());
	}

	public BentoFrame(GraphicsConfiguration gc) {
		super(gc);
		initialize(getUnusedTitle());
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
		
		// the mergegrid needs a minimum size so we don't get weird roundoff
		// behavior so don't remove this
		this.setMinimumSize(new Dimension(320, 240));
		myGrid = new MergeGrid();
		this.getContentPane().add(
				myGrid, 
				BorderLayout.CENTER);
		CameraViewer cv = new CameraViewer(JVisionEngine.CAMERA_NAME);
		
		myGrid.addColumn(0, cv.getPreferredSize().width);
		myGrid.addRow(0, cv.getPreferredSize().height);
		try {
			myGrid.setCell(cv, 0, 0, 1, 1);
		} catch (ItsBentoBoxesNotBentoTetrisException ex) {
			Logger.getLogger(BentoFrame.class.getName()).log(Level.SEVERE, null, ex);
		}
		
		setupMenus();
		
		this.pack();
		this.setVisible(true);
		BentoLauncher.getDefaultLauncher().addWindow(this);
	}
	
	private void setupMenus()
	{
		JMenuItem item;
		JMenuBar bar = new JMenuBar();
		
		JMenu menu = new JMenu("File");
		menu.setMnemonic(KeyEvent.VK_F);
		menu.getAccessibleContext().setAccessibleDescription("file");
		bar.add(menu);
		
		item = new JMenuItem("New",
                         KeyEvent.VK_N);
		item.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_N, ActionEvent.CTRL_MASK));
		item.getAccessibleContext().setAccessibleDescription(
				"New Bento Window");
		item.addActionListener(new ActionListener(){

			@Override
			public void actionPerformed(ActionEvent e) {
				BentoFrame bf = new BentoFrame(BentoFrame.getUnusedTitle());
			}
		});
		menu.add(item);
		
		item = new JMenuItem("Open",
                         KeyEvent.VK_O);
		item.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_O, ActionEvent.CTRL_MASK));
		item.getAccessibleContext().setAccessibleDescription(
				"New Bento Window");
		menu.add(item);
		
		this.setJMenuBar(bar);
	}
}
