package org.friendularity.gui.freckle;


import org.friendularity.ancient.FaceRecServer.FaceRecServer;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;

import org.cogchar.platform.util.TimeUtils;
import org.cogchar.sight.api.obs.OpenCVImage;

public class ServerControlPanel extends JPanel implements WindowListener {
	public ServerControlPanel(String config, ServerVizUI parent) {
		m_parent = parent;
		createComponents();
		
		m_server = new FaceRecServer(config);
		m_server.start();
	}
	
	public void windowClosed(WindowEvent e) {
		m_server.shutdown();
	}
	private OpenCVImage getQueryOpenCVImage() {
		return m_imgPanel.getQueryOpenCVImage();
	}
	private void createComponents() {
		Box contents = Box.createVerticalBox();
		contents.add(new JLabel("Query Image: "));

		m_imgPanel = new ImagePanel();
		m_imgPanel.setPreferredSize(new Dimension(320,320));
		contents.add(m_imgPanel);
		m_AddIfUnknown = new JCheckBox("Add if unknown");
		contents.add(m_AddIfUnknown);
		m_LogArea = new JTextArea(10,20);
		contents.add(m_LogArea);
		
		Box controls = Box.createVerticalBox();

		JButton matchButton   = new JButton(new AbstractAction("Match") {
        	public void actionPerformed(ActionEvent e) {
				System.out.println("Match [START] at " + TimeUtils.currentTimeMillis());
				OpenCVImage queryOCVI = getQueryOpenCVImage();
				System.out.println("Match [GOT IMAGE] at " + TimeUtils.currentTimeMillis());
        		String answer = m_server.matchPersonInDefaultPop(queryOCVI);
				System.out.println("Match [END] at " + TimeUtils.currentTimeMillis());
        		if ( answer.isEmpty() ) {
        			m_LogArea.append("No match found\n");
        		} else {
        			m_LogArea.append("Found: " + answer + "\n");
        		}
        	}
        });
		JButton matchOrAddButton   = new JButton(new AbstractAction("Match Or Add") {
        	public void actionPerformed(ActionEvent e) {
				System.out.println("Match or Add[START] at " + TimeUtils.currentTimeMillis());
				OpenCVImage queryOCVI = getQueryOpenCVImage();
				System.out.println("Match or Add[GOT IMAGE] at " + TimeUtils.currentTimeMillis());
        		String answer = m_server.matchOrAddPersonInDefaultPop(queryOCVI);
				System.out.println("Match or Add[END] at " + TimeUtils.currentTimeMillis());
        		m_LogArea.append("Found: " + answer + "\n");
        	}
        });
		JButton addButton   = new JButton(new AbstractAction("Add Person") {
        	public void actionPerformed(ActionEvent e) {
        		String name = m_addNameTextField.getText();
        		if ( ! name.isEmpty() ) {
					System.out.println("Add Person [START] at " + TimeUtils.currentTimeMillis());
					OpenCVImage queryOCVI = getQueryOpenCVImage();
					System.out.println("Add Person [GOT IMAGE] at " + TimeUtils.currentTimeMillis());
					m_server.addNamedPersonToDefaultPop(queryOCVI, name);
					System.out.println("Add Person [END] at " + TimeUtils.currentTimeMillis());
        			m_LogArea.append("Added: " + name + "\n");
        		} else {
					m_LogArea.append("No name found - add skipped\n");
				}
        	}
        });
		JButton removeButton   = new JButton(new AbstractAction("Remove Person") {
        	public void actionPerformed(ActionEvent e) {
        		String name = m_removeNameTextField.getText();
        		if ( ! name.isEmpty() ) {
        			m_server.removePersonFromDefaultPop(name);
        			m_LogArea.append("Removed: " + name + "\n");
        		}
        	}
        });
		JButton listButton   = new JButton(new AbstractAction("List population") {
        	public void actionPerformed(ActionEvent e) {
        		m_LogArea.append("Population List: \n");
        		String[] names = m_server.listDefaultPopulation();
        		for ( String name : names ) {
        			m_LogArea.append("\t" + name + "\n");
        		}
        	}
        });
		JButton loadPopButton   = new JButton(new AbstractAction("Load population") {
        	public void actionPerformed(ActionEvent e) {
                JFileChooser fc = new JFileChooser(m_parent.m_lastOpenedDirectory);
                fc.setFileFilter(new FileFilter() {
                	public boolean accept(File f) {
                        return f.isDirectory() || f.getName().toLowerCase().endsWith(".fvp");
                    }
                    
                    public String getDescription() {
                        return "Image files";
                    }
                });
                int returnVal = fc.showOpenDialog(m_parent.m_view);
                        
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = fc.getSelectedFile();
                    if ( ! file.getPath().endsWith("fvp") ) {
                    	File t = null;
                    	for ( File i : file.listFiles() ) {
                    		
                    	}
                    	if ( t != null ) {
                    		file = t;
                    	} else {
                    		file = null;
                    	}
                    }
                    
                    if ( file != null ) {
                    	//This is where a real application would open the file.
                    	m_parent.m_lastOpenedDirectory = file.getParent();
                    	m_server.loadPopulationAndReplaceDefault(file.getAbsolutePath());
                    	m_LogArea.append("Loading population from: " + file.getName() + "\n");
                    }
                }
        	}
        });

		m_addNameTextField = new JTextField(12);
		m_removeNameTextField = new JTextField(12);
		
		controls.add(matchButton);
		controls.add(matchOrAddButton);
		controls.add(addButton);
		controls.add(m_addNameTextField);
		controls.add(removeButton);
		controls.add(m_removeNameTextField);
		controls.add(loadPopButton);
		controls.add(listButton);

		Box layout = Box.createHorizontalBox();
		layout.add(contents);
		layout.add(controls);
		
		add(layout);
		setTransferHandler(new FaceTransferHandler());
	}

	public void setQueryImage(Image img) {
		m_imgPanel.setImage(ServerVizUI.toBufferedImage(img));
		m_imgPanel.repaint();
	}
	
	public FaceRecServer getServer() {
		return m_server;
	}
	
	// other stupid window listener functions
	public void windowActivated(WindowEvent e) {}
	public void windowClosing(WindowEvent e) {}
	public void windowDeactivated(WindowEvent e) {}
	public void windowDeiconified(WindowEvent e) {}
	public void windowIconified(WindowEvent e) {}
	public void windowOpened(WindowEvent e) {} 
	
	private FaceRecServer m_server;
	private ImagePanel m_imgPanel;
	private JCheckBox m_AddIfUnknown;
	private JTextArea m_LogArea;
	private JTextField m_addNameTextField;
	private JTextField m_removeNameTextField;
	private ServerVizUI m_parent;
}
