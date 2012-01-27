package org.friendularity.gui.freckle;

import org.friendularity.ancient.FaceRecServer.FaceRecPopulation;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Label;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import org.cogchar.sight.vision.OpenCVImage;
import org.cogchar.sight.vision.PortableImage;

public class PopulationPanel extends JPanel {
	public PopulationPanel(ServerVizUI app) {
		m_app = app;
		setLayout(new GridLayout(0,1));
		JPanel shim = new JPanel();
		JScrollPane sPane = new JScrollPane(shim);
		//, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED) 
		sPane.setAutoscrolls(true);
		add(sPane);
		
		m_self = this;
		m_shim = shim;
		m_location = null;
		m_people = new HashMap();
	    
	    createComponents();
	}

	public PopulationPanel(File loc) {
		setLayout(new GridLayout(0,1));
		JPanel shim = new JPanel();
		JScrollPane sPane = new JScrollPane(shim);
		//, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED) 
		sPane.setAutoscrolls(true);
		add(sPane);
		
		m_self = this;
		m_shim = shim;
		m_location = loc;
		m_people = new HashMap();
	    File[] files = loc.listFiles();
	    
	    // This filter only returns directories
	    FileFilter fileFilter = new FileFilter() {
	        public boolean accept(File file) {
	            return file.isDirectory();
	        }
	    };
	    files = loc.listFiles(fileFilter);
	    
	    for ( File subdir : files ) {
		    // This filter only returns directories
		    FileFilter picFilter = new FileFilter() {
		        public boolean accept(File file) {
		            return file.getName().endsWith(".bmp");
		        }
		    };
		    m_people.put(subdir.getName(), subdir.listFiles(picFilter));
	    }
	    
	    createComponents();
	}
	
	public File[] getPics(String name) {
		return (File[])m_people.get(name);
	}
	
	private void createComponents() {
		m_shim.setLayout(new BoxLayout(m_shim, BoxLayout.Y_AXIS));
		m_shim.add(new Label("Population view (enter name and press return, then drop images onto the new component)"));
		if ( m_location != null ) {
			m_shim.add(new Label(m_location.getPath()));
		}
		m_shim.add(Box.createVerticalGlue());

		for ( Object name : m_people.keySet() ) {
		    m_shim.add(new PersonPanel((String)name, m_self));
		}
		
		m_addNameTextBox = new JTextField(20);
		m_addNameTextBox.setAction(new AbstractAction("Add name") {
			public void actionPerformed(ActionEvent e) {
				String name = m_addNameTextBox.getText();
				
				m_self.addPerson(new PersonPanel(name, m_self), m_shim.getComponentCount()-2);
				m_self.validate();
				m_self.repaint();
				
				m_addNameTextBox.setText("");
			}
		});
		Dimension size = m_addNameTextBox.getPreferredSize();
	    //size.width = Short.MAX_VALUE;
		m_addNameTextBox.setMaximumSize(size);
		m_shim.add(m_addNameTextBox);
		
		Box actionsButtonBox = Box.createHorizontalBox();
		actionsButtonBox.add(new JButton(new AbstractAction("Save Population") {
			public void actionPerformed(ActionEvent e) {
				m_self.savePopulation();
			}
		}));
		actionsButtonBox.add(new JButton(new AbstractAction("Compile Population") {
			public void actionPerformed(ActionEvent e) {
				m_self.compilePopulation();
			}
		}));
		m_shim.add(actionsButtonBox);
		
		setVisible(true);
        repaint();
	}
	
	public void removePerson(PersonPanel p) {
		m_shim.remove(p);
	}
	
	public void addPerson(PersonPanel p) {
		m_shim.add(p);
	}
	
	public void addPerson(PersonPanel p, int position) {
		m_shim.add(p, position);
	}
	
	public void savePopulation() {
		if ( m_location == null ) {
            JFileChooser fc = new JFileChooser();
            fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            int returnVal = fc.showOpenDialog(m_self);
                    
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                m_location = fc.getSelectedFile();
            } else {
            	return;
            }
		}
		
		if ( ! m_location.exists() ) {
			// create the directory of one is not there
			m_location.mkdir();
		} else {
			if ( ! m_location.isDirectory() ) {
				JOptionPane.showMessageDialog(null, "Invalid save location: Must be a directory",
						"Save Error", JOptionPane.ERROR_MESSAGE);
			}
		}
				
		for ( Component c : m_shim.getComponents() ) {
			PersonPanel person = null;
			try {
				person = (PersonPanel)c; 
			} catch (ClassCastException e) {
			}
			
			if ( person != null ) {
				String pName = person.getName();
				String pDirPath = m_location.getPath() + "\\" + pName;
				System.out.println("Finding/Creating directory for " + pName + " at " + pDirPath);
				File newDir = new File(pDirPath );
				if ( ! newDir.exists() ) {
					System.out.println("Couldn't find " + pDirPath + " - creating!");
					newDir.mkdir();
				} else {
					if ( ! newDir.isDirectory() ) {
						JOptionPane.showMessageDialog(null, "File \"" + newDir.getPath() + "\" blocks creating a "
								+ "directory of the same name. Please rename or move this file.",
								"Save Error", JOptionPane.ERROR_MESSAGE);
						return;
					} else {
						System.out.println("Found " + pDirPath);
					}
				}
				
				for ( File old : newDir.listFiles() ) {
					String oldAbsPath = old.getAbsolutePath();
					System.out.println("Found old file at: " + oldAbsPath);
					if ( oldAbsPath.endsWith(".bmp") ) {
						System.out.println("Deleting old .bmp file at: " + oldAbsPath);
						old.delete();
					} else {
						System.out.println("Ignoring old (non .bmp) file at: " + oldAbsPath);
					}
				}

				int imageNum = 0;
				for ( Object o_img : person.getImages().toArray() ) {
					Image img = (Image)o_img;
					// Stu sez: This nImg does not appear to be used, other than to write bitmaps
					boolean flippedVertical = true;
					PortableImage pimg = new PortableImage(img, flippedVertical);
					OpenCVImage nImg = pimg.fetchOpenCVImage();
					String imagePathTail = "image" + imageNum++ + ".bmp";
					String imagePathFull = newDir.getPath() + "\\" + imagePathTail;
					System.out.println("Saving image file to: " + imagePathFull);
					File imageName = new File(imagePathFull);
					try {
						if ( img != null ) {
							ImageIO.write((BufferedImage)img, "BMP", imageName);
						}
					} catch (IOException e) {
						JOptionPane.showMessageDialog(null, "Error writing image file:\n" + e.getMessage(),
								"Save Error", JOptionPane.ERROR_MESSAGE);
					}
				}
			}
		}
		System.out.println("Population save complete");
	}
	
	public void compilePopulation() {
		if ( m_location == null ) {
			JOptionPane.showMessageDialog(null, "You must save the population first",
					"Compile Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		System.out.println("compilePopulation() - Creating new empty population");

		FaceRecPopulation pop = new FaceRecPopulation(); 
		
		System.out.println("Created pop record OK, now looping through people");
		
		for ( Component c : m_shim.getComponents() ) {
			PersonPanel person = null;
			if (c instanceof PersonPanel) {
				person = (PersonPanel) c;
			} else {
				continue;
			}
			String enrollName = person.getName();
			ArrayList images = new ArrayList(); 

			Object rawImages[] = person.getImages().toArray();
			System.out.println("Found " + rawImages.length + " raw images for person " + enrollName);
			for ( Object o_img :  rawImages) {
				Image rawImage = (Image) o_img;
				boolean flippedVertical = true;
				PortableImage pimg = new PortableImage(rawImage, flippedVertical);
        		OpenCVImage ocvi = pimg.fetchOpenCVImage();
				images.add(ocvi);
			}
			OpenCVImage[] enrollOCVIs = (OpenCVImage[])images.toArray(new OpenCVImage[0]);

			System.out.println("Enrolling " + enrollName + " with " + enrollOCVIs.length + " images");
			pop.enroll(enrollName, enrollOCVIs);

		}
		String popPath = m_location.getPath() + "/compiledPopulation.fvp";
		System.out.println("Saving newly compiled population to " + popPath);
		pop.saveToFile(popPath);		
	}

	private PopulationPanel m_self;
	private JPanel m_shim;
	private File m_location;
	private HashMap m_people;
	private JTextField m_addNameTextBox;
	private ServerVizUI m_app; 
}

class PersonPanel extends Box {
	public PersonPanel(String name, PopulationPanel parent) {
		super(BoxLayout.X_AXIS);
		m_self = this;
		m_parent = parent;
		m_name = name;
		m_images = new ArrayList();
		
		createComponents();
		setVisible(true);
		setTransferHandler(new FaceTransferHandler());
	}
	
	private void createComponents() {
		Box leftSide = Box.createVerticalBox();
		leftSide.add(Box.createVerticalGlue());
		
		JTextField nameBox = new JTextField(m_name, 20);
		Dimension size = nameBox.getPreferredSize();
	    //size.width = Short.MAX_VALUE;
	    nameBox.setMaximumSize(size);
		leftSide.add(nameBox);
		
		JButton removeButton =new JButton(new AbstractAction("Remove") {
			public void actionPerformed(ActionEvent e) {
				m_parent.removePerson(m_self);
				m_parent.validate();
				m_parent.repaint();
			}
		});
		size = removeButton.getPreferredSize();
	    //size.width = Short.MAX_VALUE;
	    removeButton.setMaximumSize(size);
		leftSide.add(removeButton);
		leftSide.add(Box.createVerticalGlue());
		add(leftSide);
		
        Iterator readers = ImageIO.getImageReadersByFormatName("bmp");
        ImageReader reader = (ImageReader)readers.next();
        File[] pics = m_parent.getPics(m_name);
        if ( pics != null ) {
        	for (File pic : pics ) {
        		try {
        			ImageInputStream iis = ImageIO.createImageInputStream(pic);
        			reader.setInput(iis, true);
        			Image image = reader.read(0);
        			m_images.add(image);
        			add(new RemovableImageIcon(image, m_self));
        		} catch (IOException e) {
        		}
        	}
        }
        
        setTransferHandler(new FaceTransferHandler());
	}
	
	public void addImage(Image image) {
		m_images.add(image);
		add(new RemovableImageIcon(image, m_self));
		m_parent.validate();
		m_parent.repaint();
	}
	
	public String getName() { return m_name; }
	public ArrayList getImages() { 
		if ( m_images.size() != m_self.getComponentCount()-1 ) {
			ArrayList newImages = new ArrayList();
			
			for ( Component i : m_self.getComponents() ) {
				try {
					RemovableImageIcon icon = (RemovableImageIcon)i;
					newImages.add(icon.getImage());
				} catch (ClassCastException e) {
					
				}
			}
			
			m_images = newImages;
		}	
		return m_images;
	}
	
	private PersonPanel m_self;
	private PopulationPanel m_parent;
	private String m_name;
	private ArrayList m_images;
}

class RemovableImageIcon extends JPanel {
	public RemovableImageIcon(Image img, JComponent parent) {
		m_parent = parent;
		m_self = this;
		
		ImageIcon icon = new ImageIcon(img);
		add(new JLabel(icon));

		//...where instance variables are declared:
		JPopupMenu popup;

	    //...where the GUI is constructed:
	    //Create the popup menu.
	    popup = new JPopupMenu();
	    JMenuItem menuItem = new JMenuItem(new AbstractAction("Remove") {
	    	public void actionPerformed(ActionEvent e) {
	    		m_parent.remove(m_self);
	    		m_parent.repaint();
	    	}
	    });
	    popup.add(menuItem);

	    //Add listener to components that can bring up popup menus.
	    MouseListener popupListener = new PopupListener(popup);
	    addMouseListener(popupListener);
	} 
	
	Image getImage() { return m_image; }
	
	private Image m_image;
	private JComponent m_parent;
	private JComponent m_self;
}
	
class PopupListener extends MouseAdapter {
	public PopupListener(JPopupMenu popup) {
		m_popup = popup;
	}
	
    public void mousePressed(MouseEvent e) {
        maybeShowPopup(e);
    }

    public void mouseReleased(MouseEvent e) {
	    maybeShowPopup(e);
	}

	private void maybeShowPopup(MouseEvent e) {
	    if (e.isPopupTrigger()) {
	        m_popup.show(e.getComponent(),
	                   e.getX(), e.getY());
	    }
	}
	
	private JPopupMenu m_popup;
}
