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


import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import org.friendularity.jvision.engine.CVChainManager;
import java.awt.Component;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.UIManager;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.friendularity.jvision.engine.JVisionEngine;
import org.friendularity.jvision.engine.JVisionRDF;
import org.friendularity.jvision.filters.FilterInfo;
import static org.friendularity.jvision.gui.JVisionFrame.theLogger;

/**
 *
 * @author Annie
 */
public class FilterBox extends JPanel {
	private JMenuBar		myMenuBar;
    private FilterTree tree;
	private JPanel listPanel;
	
	private CVChainControl selectedCVChainControl = null;
	
    //Optionally set the look and feel.
    private static boolean useSystemLookAndFeel = false;
	// Instead of a dirty bit we just hang on to the old model
	private Model last_saved_model = JVisionRDF.createDefaultJVisionModel();
	// the file we saved it under
	private File last_saved_name = null;

    public FilterBox() {
        super(new GridLayout(1,0));

        //Create a tree that allows one selection at a time.
        tree = new FilterTree(FilterTree.nodeFactory(FilterTree.ALL_FILTERS));
        
        //Create the scroll pane and add the tree to it. 
        JScrollPane treeView = new JScrollPane(tree);

		listPanel = new JPanel();
		listPanel.setLayout(new BoxLayout(listPanel, BoxLayout.LINE_AXIS));

		listPanel.setBackground(listPanel.getBackground().darker());
		
        JScrollPane filterListsView = new JScrollPane(listPanel);

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
	
private void removeAllCVChains() {
		for(int i = 0 ; i < listPanel.getComponentCount() ; ) {
			if(listPanel.getComponent(i) instanceof CVChainControl) {
				CVChainControl cvcc = (CVChainControl)(listPanel.getComponent(i));
				CVChainManager.getDefaultManager().remove(this, cvcc);
				listPanel.remove(cvcc);
			}
		}
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
				theLogger.debug("New");
				if(!saveAsViaDirtyBit())return;
				removeAllCVChains();
				last_saved_model = JVisionRDF.createDefaultJVisionModel();
				ImageStreamBroker.getDefaultImageStreamBroker().removeAllOfflineStreams();
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Open...", KeyEvent.VK_O);
		menuItem.addActionListener(new ActionListener(){
			@Override	public void actionPerformed(ActionEvent arg0) {
				if(!saveAsViaDirtyBit())return;
				
				// offer the open dialog
				final JFileChooser fc = new JFileChooser();
				fc.setDialogType(JFileChooser.OPEN_DIALOG);
				fc.setFileFilter(new FileNameExtensionFilter("flo files", "flo", "ttl"));
				if(last_saved_name != null) {
					fc.setCurrentDirectory(last_saved_name);
					fc.setSelectedFile(last_saved_name);
				} else {
					fc.setCurrentDirectory(new File(System.getProperty("user.dir")));
				}
				if(fc.showOpenDialog(FilterBox.this) == JFileChooser.APPROVE_OPTION) {
					last_saved_name = fc.getSelectedFile();

					String path = "";
					try {
						path = last_saved_name.getCanonicalPath();
						FileInputStream fis = new FileInputStream(FilterBox.this.last_saved_name);
						Model M = JVisionRDF.createDefaultJVisionModel();
						M.read(fis, JVisionRDF.CV_PREFIX, "TURTLE");
						fis.close();
						last_saved_model = M;
						removeAllCVChains();
						createUIFromModel(M);
						ImageStreamBroker.getDefaultImageStreamBroker().removeAllOfflineStreams();
					} catch (IOException ex) {
						JOptionPane.showMessageDialog(FilterBox.this, "Cannot Open File", "Can't open " + path, 
								JOptionPane.WARNING_MESSAGE);
					}
				}
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Save", KeyEvent.VK_S);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				if(last_saved_name == null) {
					saveAs();
				} else {
					String path = "";
					try {
						path = FilterBox.this.last_saved_name.getCanonicalPath();
						FileWriter fw = new FileWriter(FilterBox.this.last_saved_name);
						Model M = getRDFModel();
						M.write(fw,
								"TURTLE",
								JVisionRDF.CV_PREFIX);

						last_saved_model = M;
						fw.close();
					} catch (IOException ex) {
						JOptionPane.showMessageDialog(FilterBox.this, "Can't Save", 
								"Can't save in " + path, JOptionPane.WARNING_MESSAGE);
					}

				}
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Save As...", KeyEvent.VK_A);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				saveAs();
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Quit...", KeyEvent.VK_Q);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				saveAsViaDirtyBit();
				JVisionEngine.getDefaultJVisionEngine().requestQuit();
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
				if(selectedCVChainControl == null)
					return;
				
				CVChainManager.getDefaultManager().remove(FilterBox.this, selectedCVChainControl);
			}
		});
		menu.add(menuItem);
		myMenuBar.add(menu);
		
		menu = new JMenu("Filter");
		
		menuItem = new JMenuItem("Add...", KeyEvent.VK_A);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				if(selectedCVChainControl == null)
					return;
				
				FilterInfo fi = tree.getCurrentFilterSelectionOrNull();
				if(fi == null)
					return;
				
				selectedCVChainControl.getFilterSequence().add(fi.createInstance());
			}
		});
		menu.add(menuItem);

		menuItem = new JMenuItem("Remove", KeyEvent.VK_R);
		menuItem.addActionListener(new ActionListener(){
			@Override	
			public void actionPerformed(ActionEvent arg0) {
				selectedCVChainControl.removeSelectedFilter();
			}
		});
		menu.add(menuItem);
		
		menuItem = new JMenuItem("Parameters...", KeyEvent.VK_P);
		menuItem.addActionListener(new ActionListener(){

			@Override
			public void actionPerformed(ActionEvent e) {
				selectedCVChainControl.showParametersOfSelectedFilter();
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
	
	public void setSelectedChainControl(CVChainControl current) {
		selectedCVChainControl = current;

		int n = listPanel.getComponentCount();
		
		for(int i = 0 ; i < n ; i++) {
			Component c = listPanel.getComponent(i);
			
			if(c instanceof CVChainControl) {
				c.repaint();
			}
		}
		
	}

	public void addCVChainControl(CVChainControl cvChainControl) {

		listPanel.add(cvChainControl);
		setSelectedChainControl(cvChainControl);
		
		cvChainControl.revalidate();
		listPanel.repaint();
	}

	CVChainControl getSelectedCVChainControl() {
		if(selectedCVChainControl != null)
			return selectedCVChainControl;
		
		int n = listPanel.getComponentCount();
		
		CVChainControl candidate = null;
		
		for(int i = 0 ; i < n ; i++) {
			Component c = listPanel.getComponent(i);
			
			if(c instanceof CVChainControl) {
				if(candidate != null)  // if there's more than one cvChainControl we don't have candidate
					return null;
				
				candidate = (CVChainControl)c;
			}
		}

		selectedCVChainControl = candidate;
		
		return selectedCVChainControl;
	}

	public void removeChainControl(CVChainControl cvcc) {
		listPanel.remove(cvcc);
		
		if(selectedCVChainControl == cvcc)
			selectedCVChainControl = null;
		
		this.revalidate();
		this.repaint();
	}

	/**
	 * save off a dirty model before continuing
	 * 
	 * 
	 * @return true if we should continue
	 */
	private boolean saveAsViaDirtyBit() {
		Model M = getRDFModel();
		
		// TBD does this work if you change the file name?
		// deskcheck through the various UI actions
		if(FilterBox.this.last_saved_model == null || 
				!last_saved_model.isIsomorphicWith(M)) {
			Object[] options = {
				"Save", "Don't Save", "Cancel"
			};
			
			String filename = "Untitled";
			if(this.last_saved_name != null)
				filename = last_saved_name.getName();
			
			int response = JOptionPane.showOptionDialog(this, 
					"Do you want to save changes to " + filename, 
					"FilterBox", 
					JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.PLAIN_MESSAGE,
					null,   // icon
					options,
					options[2]);
			if(response == 2)return false; // we cancelled
			if(response == 0) {
				return saveAs();
			}  // if save button chosen
		}  // if we were dirty
		
		return true;
	}

	private void createUIFromModel(Model M) {
		Property p = M.createProperty(JVisionRDF.RDF_PREFIX + "type");
		Resource o = M.createResource(JVisionRDF.FLO_PREFIX + "CVChain");
		for(ResIterator i = M.listResourcesWithProperty(p, o);
				i.hasNext(); ) {
			Resource cvchain = i.next();
			
			CVChainManager.getDefaultManager().buildChain(this, cvchain, M);
		}
		listPanel.revalidate();
	}
			
	private Model getRDFModel() {
		Model M = JVisionRDF.createDefaultJVisionModel();
		
		Component[] cs = listPanel.getComponents();
		for(int i = 0 ; i < cs.length ; i++) {
			if(cs[i] instanceof CVChainControl) {
				((CVChainControl)(cs[i])).addSelfToModel(M);
			}
		}
		
		return M;
	}

	/**
	 * save off to a user specified file
	 * 
	 * @return true if the user saved and we should continue
	 */
	private boolean saveAs() {
		Model M = getRDFModel();
		
		final JFileChooser fc = new JFileChooser();
		fc.setDialogType(JFileChooser.SAVE_DIALOG);
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fc.setFileFilter(new FileNameExtensionFilter("flo files", "flo", "ttl"));

		if (FilterBox.this.last_saved_name != null) {
			fc.setCurrentDirectory(FilterBox.this.last_saved_name);
			fc.setSelectedFile(last_saved_name);
		} else {
			fc.setSelectedFile(new File("*.flo"));
			fc.setCurrentDirectory(new File(System.getProperty("user.dir")));
		}

		if(fc.showSaveDialog(FilterBox.this) == JFileChooser.APPROVE_OPTION) {
			FilterBox.this.last_saved_name = fc.getSelectedFile();
			String path = "";
			try {
				path = FilterBox.this.last_saved_name.getCanonicalPath();
				
				if (FilterBox.this.last_saved_name.exists()) {
					int res  = JOptionPane.showConfirmDialog(this, 
							"File Exists", 
							path + " Exists, OK to overwrite?",
							JOptionPane.OK_CANCEL_OPTION);
					if(res == JOptionPane.CANCEL_OPTION || res == JOptionPane.NO_OPTION)return false;
				}
						
				FileWriter fw = new FileWriter(FilterBox.this.last_saved_name);
				M.write(fw,
						"TURTLE",
						JVisionRDF.CV_PREFIX);

				last_saved_model = M;
				fw.close();
			} catch (IOException ex) {
				JOptionPane.showMessageDialog(this, "Can't Save", "Can't save in " + path,
						JOptionPane.WARNING_MESSAGE);
				
				return false;
			}

			return true;
		} else {
			return false;
		}
	}
}

// From freenode.net ##prolog IRC
// [23:09] <groovy2shoes> Swing is short for SwampThing
// [23:09] <groovy2shoes> pretty sure
