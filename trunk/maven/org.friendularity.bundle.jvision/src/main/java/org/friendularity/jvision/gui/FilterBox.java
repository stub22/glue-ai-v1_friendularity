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
 * 
 * * Copyright (c) 1995, 2008, Oracle and/or its affiliates. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Oracle or the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.friendularity.jvision.gui;


import org.friendularity.jvision.engine.CVChainManager;
import java.awt.Component;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.UIManager;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import java.net.URL;
import java.io.IOException;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import javax.swing.DropMode;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import org.friendularity.jvision.engine.JVisionEngine;
import org.friendularity.jvision.filters.BananaDetector;
import org.friendularity.jvision.filters.*;
import static org.friendularity.jvision.gui.DemoFrame.theLogger;

/**
 *
 * @author Annie
 */
public class FilterBox extends JPanel {
	private JMenuBar		myMenuBar;
	
	// its OK to promote these to FilterList and FilterTree
	// I havent' needed it yet
    private JList filterList;
    private JTree tree;

    //Optionally set the look and feel.
    private static boolean useSystemLookAndFeel = false;

    public FilterBox() {
        super(new GridLayout(1,0));

        //Create a tree that allows one selection at a time.
        tree = new FilterTree(FilterTree.nodeFactory(FilterTree.ALL_FILTERS));
        
        //Create the scroll pane and add the tree to it. 
        JScrollPane treeView = new JScrollPane(tree);

        //Create (for now a single) list pane to receive the drop
        filterList = new FilterList();
     //   filterList.setEnabled(true);
	//	filterList.setDragEnabled(true);
		
		FilterSequence model = new FilterSequence();
		
		filterList.setModel(model);
		
		// these can only be done after everything else is up.
	//	model.addOrReplaceByClass(new FaceDetector());
	//	model.addOrReplaceByClass(new Blur());

        JScrollPane filterListsView = new JScrollPane(filterList);

        //Add the scroll panes to a split pane.
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setTopComponent(treeView);
        splitPane.setBottomComponent(filterListsView);

        Dimension minimumSize = new Dimension(100, 50);
        filterListsView.setMinimumSize(minimumSize);
        treeView.setMinimumSize(minimumSize);
        splitPane.setDividerLocation(200); 
        splitPane.setPreferredSize(new Dimension(800, 500));

        //Add the split pane to this panel.
        add(splitPane);
    }
private  void setupMenus(JFrame frame) {
		
		JMenu menu;
		JMenuItem menuItem;
		
		myMenuBar = new JMenuBar();

		//Build the first menu.
		menu = new JMenu("File");
		menu.setMnemonic(KeyEvent.VK_F);
		myMenuBar.add(menu);
    
		menuItem = new JMenuItem("New", KeyEvent.VK_N);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Someday New");
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Open...", KeyEvent.VK_O);
		menuItem.addActionListener(new ActionListener(){
			@Override	public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("someday Open");
			}
		});
		menu.add(menuItem);
		
				menuItem = new JMenuItem("Save", KeyEvent.VK_S);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Someday Save");
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Save As...", KeyEvent.VK_A);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Someday Save As...");
			}
		});
		menu.add(menuItem);
		
		//Build second menu in the menu bar.
		menu = new JMenu("CVChain");
		menu.setMnemonic(KeyEvent.VK_F);
		
		menuItem = new JMenuItem("Add...", KeyEvent.VK_A);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("add a FilterSeq...");
				CVChainInfoDialog cvcid = new CVChainInfoDialog(FilterBox.this, CVChainManager.getDefaultManager());
				cvcid.setVisible(true);
			}
		});
		menu.add(menuItem);

		menuItem = new JMenuItem("Remove", KeyEvent.VK_R);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Someday remove a FilterSeq...");
			}
		});
		menu.add(menuItem);
		myMenuBar.add(menu);
		
		menu = new JMenu("Filter");
		
		menuItem = new JMenuItem("Add...", KeyEvent.VK_A);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Someday Add a Filter...");
				theLogger.debug("For now add a couple arbitrary ones...");
				FilterSequence fs = (FilterSequence)(filterList.getModel());
				fs.addOrReplaceByClass(new FaceDetector());
				fs.addOrReplaceByClass(new Blur());
			}
		});
		menu.add(menuItem);

		menuItem = new JMenuItem("Remove", KeyEvent.VK_R);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				theLogger.debug("Someday remove a Filter...");
			}
		});
		menu.add(menuItem);
		
		myMenuBar.add(menu);

		frame.setJMenuBar(myMenuBar);
	}
	
    /**
     * Create the GUI and show it.  For thread safety,
     * this method should be invoked from the
     * event dispatch thread.
     */
    private static void createAndShowGUI() {
        if (useSystemLookAndFeel) {
            try {
                UIManager.setLookAndFeel(
                    UIManager.getSystemLookAndFeelClassName());
            } catch (Exception e) {
                System.err.println("Couldn't use system look and feel.");
            }
        }

        //Create and set up the window.
        JFrame frame = new JFrame("Filter Box");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        //Add content to the window.
		FilterBox fb = new FilterBox();
		
        frame.add(fb);
		fb.setupMenus(frame);

        //Display the window.
        frame.pack();
        frame.setVisible(true);
    }

    public static void showFilterBox() {
        //Schedule a job for the event dispatch thread:
        //creating and showing this application's GUI.
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }

	Frame getFrame() {
		Component p;
		for(p = this; !(p instanceof Frame) ; ) {
			p = p.getParent();
		}
		return (Frame)p;
			
	}
}

// From freenode.net ##prolog IRC
// [23:09] <groovy2shoes> Swing is short for SwampThing
// [23:09] <groovy2shoes> pretty sure