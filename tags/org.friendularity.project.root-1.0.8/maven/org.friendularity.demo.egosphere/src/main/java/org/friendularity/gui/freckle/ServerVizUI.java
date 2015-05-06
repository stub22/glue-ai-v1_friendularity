package org.friendularity.gui.freckle;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Label;
import java.awt.Transparency;
import java.awt.event.ActionEvent;
import java.awt.event.WindowListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.PixelGrabber;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.swing.filechooser.FileFilter;
import javax.swing.AbstractAction;
import javax.swing.GroupLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.LayoutStyle;

import org.cogchar.zzz.nwrap.core.EmptyEngine;
import org.cogchar.sight.api.obs.OpenCVImage;
import org.cogchar.sight.api.obs.PortableImage;

public class ServerVizUI {
    public ServerVizUI () {
        m_lastOpenedDirectory = "";
        m_view = null;
        m_engine = new EmptyEngine();
        m_scaleImage = true;
        m_self = this;
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        //Schedule a job for the event-dispatching thread:
        //creating and showing this application's GUI.
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                ServerVizUI ui = new ServerVizUI();
                ui.launch();
            }
        });
    }

    public void launch() {
        // Create the gui
        JFrame frame = new JFrame("Face Recognition Server Manager");
        m_view = frame;
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // Create components
        // Main title label
        Label mainTitleLabel = new Label("Face Recognition Server Manager");
        mainTitleLabel.setFont(new Font("Serif", Font.PLAIN, 14));

        // Action buttons
        JButton launchServerButton   = new JButton(new AbstractAction("Launch Server") {
        	public void actionPerformed(ActionEvent e) {
        		int port = Integer.parseInt(m_portTextField.getText());
       // launchInAFrame("Server Panel", new ServerControlPanel("FaceVacsConfig:C:\\FVSDK_6_1_0\\etc\\frsdk.cfg\n"
				launchInAFrame("Server Panel", new ServerControlPanel(
					"FaceVacsConfig:" + Constants.fvConfigPath + "\n"
        				+ "ServerPort:" + port + "\n", m_self));
        	}
        });
        m_portTextField     = new JTextField("56001", 6);
        Label portLabel              = new Label("Port: ");
        JButton loadPopulationButton = new JButton(new AbstractAction("Load Population") {
            public void actionPerformed(ActionEvent e) { 
                JFileChooser fc = new JFileChooser(m_lastOpenedDirectory);
                fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                int returnVal = fc.showOpenDialog(m_view);
                        
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = fc.getSelectedFile();
                    //This is where a real application would open the file.
                    m_lastOpenedDirectory = file.getParent();
                    
                    m_popPanel = new PopulationPanel(file);

                    launchInAFrame("View population", m_popPanel);
                }
            }
        });
        JButton newPopulationButton = new JButton(new AbstractAction("New Population") {
            public void actionPerformed(ActionEvent e) { 
                m_popPanel = new PopulationPanel(m_self);
                launchInAFrame("View population", m_popPanel);
            }
        });

        JButton loadImageButton      = new JButton(new AbstractAction("Load Image") {
                public void actionPerformed(ActionEvent e) { 
                    JFileChooser fc = new JFileChooser(m_lastOpenedDirectory);
                    fc.setFileFilter(new FileFilter() {
                    	public boolean accept(File f) {
                            return f.isDirectory() || f.getName().toLowerCase().endsWith(".jpg")
                            	|| f.getName().toLowerCase().endsWith(".bmp")
                            	|| f.getName().toLowerCase().endsWith(".gif");
                        }
                        
                        public String getDescription() {
                            return "Image files";
                        }
                    });
                    int returnVal = fc.showOpenDialog(m_view);
                            
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        File file = fc.getSelectedFile();
                        //This is where a real application would open the file.
                        m_lastOpenedDirectory = file.getParent();
                        
                        
                        // create image reader
                        String ending = file.getName().toLowerCase().substring(file.getName().length()-3);
                        Iterator readers = ImageIO.getImageReadersByFormatName(ending);
                        ImageReader reader = (ImageReader)readers.next();
                        AffineTransform transform = new AffineTransform();
                        transform.scale(1.0, -1.0);
                        Image image = null;
                        try {
                        	ImageInputStream iis = ImageIO.createImageInputStream(file);
                        	reader.setInput(iis, true);
                        	image = reader.read(0);
                        	
                        	if ( m_scaleImage ) {
                        		m_imageCanvas.setImage(toBufferedImage(image.getScaledInstance(320, 240, Image.SCALE_AREA_AVERAGING)));                        		
                        	} else {
                        		m_imageCanvas.setImage(toBufferedImage(image));
                        	}
                        	
                        	m_imageCanvas.repaint();
                        } catch (IOException e2) {
                        }
                    }
                }
            });
        
        JButton launchCamera = new JButton(new AbstractAction("Start Camera") {
            public void actionPerformed(ActionEvent e) {
            	m_camPanel = new CameraPanel(new AbstractAction("Snap picture") {
            		public void actionPerformed(ActionEvent e) {
            			
            			m_imageCanvas.setImage(toBufferedImage(m_camPanel.getImage()));
        			    m_imageCanvas.repaint();            			
            		}
            	});
            	launchInAFrame("Camera", m_camPanel);
            }
        });


        ImagePanel imageCanvas = new ImagePanel();
        imageCanvas.setTransferHandler(new FaceTransferHandler());
        imageCanvas.setPreferredSize(new Dimension(320,320));
        JButton queryServerButton = new JButton(new AbstractAction("Query Server") {
        	public void actionPerformed(ActionEvent e) {
        		if ( m_imageCanvas.getImage() == null ) {
        			return;
        		}
        		File file = new File(m_lastOpenedDirectory, "out.bmp");
				PortableImage pimg = new PortableImage(m_imageCanvas.getImage(), true);
        		OpenCVImage cvi = pimg.fetchOpenCVImage();
        		cvi.SaveFile(file.getPath());
        	}
        });
        JButton findFacesButton = new JButton(new AbstractAction("Find Faces") {
        	public void actionPerformed(ActionEvent e) {
        		if ( m_imageCanvas.getImage() != null ) {
					PortableImage pimg = new PortableImage(m_imageCanvas.getImage(), true);
		    		OpenCVImage cvi = pimg.fetchOpenCVImage();
        			//m_imageCanvas.setBoxes(oImage.findFaces("HaarFaceDetectCascadeFile=C:\\hri\\RobotControl\\deploy\\haarcascade_frontalface_alt.xml"));
					String fdConf = "HaarFaceDetectCascadeFile=" + Constants.cvHaarPath;
					m_imageCanvas.setBoxes(cvi.findFaces(fdConf));
        			m_imageCanvas.repaint();
        		}
        	}
        });
        JButton rotateImageButton = new JButton(new AbstractAction("Rotate 90") {
			public void actionPerformed(ActionEvent e) {
				BufferedImage image = m_imageCanvas.getImage();
				if ( image == null ) {
					return;
				}
		        int w = image.getWidth(null), h = image.getHeight(null);
		        int neww = h, newh = w;
		        BufferedImage result = new BufferedImage(neww, newh, BufferedImage.TYPE_INT_RGB);
		        
		        for (int x = 0; x < neww; x++) {
		        	for (int y = 0; y < newh; y++) {
		        		result.setRGB(x, y, image.getRGB(w-y-1, x));
		        	}
		        }
		        
		        m_imageCanvas.setImage(result);
			    m_imageCanvas.repaint();
			}
        });
        m_imageCanvas = imageCanvas;
        
        m_checkBox = new JCheckBox(new AbstractAction("Scale image") {
			public void actionPerformed(ActionEvent e) {
		        m_self.setScale(m_checkBox.isSelected());
			}
        });
        m_checkBox.setSelected(true);
                
        GroupLayout layout = new GroupLayout(frame.getContentPane());
        frame.setLayout(layout);
        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(true);

        layout.setVerticalGroup(layout.createSequentialGroup()
                                .addComponent(mainTitleLabel)
                                .addGroup(layout.createSequentialGroup()
                                          .addGroup(layout.createBaselineGroup(false, true)
                                                    .addComponent(launchServerButton)
                                                    .addComponent(portLabel)
                                                    .addComponent(m_portTextField)
                                                    )
                                          .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                                    .addComponent(loadPopulationButton)
                                                    .addComponent(newPopulationButton)
                                                    )
                                          .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                                    .addComponent(loadImageButton)
                                                    .addComponent(launchCamera)
                                                    .addComponent(m_checkBox)
                                                    )
                                          )
                                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                                          .addComponent(imageCanvas)
                                          .addGroup(layout.createSequentialGroup()
                                                    .addComponent(findFacesButton)
                                                    //.addComponent(queryServerButton)
                                                    .addComponent(rotateImageButton)
                                                    )
                                          )
                                );
        layout.setHorizontalGroup(layout.createParallelGroup()
                                  .addComponent(mainTitleLabel)
                                  .addGroup(layout.createSequentialGroup()
                                            .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
                                                      .addComponent(launchServerButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      .addComponent(loadPopulationButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      .addComponent(loadImageButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      )
                                            .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
                                                      .addGroup(layout.createSequentialGroup()
                                                                .addComponent(portLabel)
                                                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED,
                                                                                 GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE)
                                                                .addComponent(m_portTextField)
                                                                )
                                                      .addComponent(newPopulationButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      .addComponent(launchCamera, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      )
                                            .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
                                                      .addComponent(m_checkBox, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      )
                                            )
                                  .addGroup(layout.createSequentialGroup()
                                            .addComponent(imageCanvas, 320, 320, 320)
                                            .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING,false)
                                                      .addComponent(findFacesButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      //.addComponent(queryServerButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      .addComponent(rotateImageButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                      )
                                            )
                                  );
            
        // Launch gui
        frame.pack();
                
        // post pack processing

        frame.setVisible(true);
        frame.repaint();
    }
    
    // This method returns a buffered image with the contents of an image
    public static BufferedImage toBufferedImage(Image image) {
    	return toBufferedImage(image, false);
    }
    public static BufferedImage toBufferedImage(Image image, boolean flipped) {
        if (image instanceof BufferedImage && ! flipped) {
            return (BufferedImage)image;
        }
    
        // This code ensures that all the pixels in the image are loaded
        image = new ImageIcon(image).getImage();
    
        // Determine if the image has transparent pixels; for this method's
        // implementation, see e661 Determining If an Image Has Transparent Pixels
        boolean hasAlpha = hasAlpha(image);
    
        // Create a buffered image with a format that's compatible with the screen
        BufferedImage bimage = null;
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        try {
            // Determine the type of transparency of the new buffered image
            int transparency = Transparency.OPAQUE;
            if (hasAlpha) {
                transparency = Transparency.BITMASK;
            }
    
            // Create the buffered image
            GraphicsDevice gs = ge.getDefaultScreenDevice();
            GraphicsConfiguration gc = gs.getDefaultConfiguration();
            bimage = gc.createCompatibleImage(
                image.getWidth(null), image.getHeight(null), transparency);
        } catch (HeadlessException e) {
            // The system does not have a screen
        }
    
        if (bimage == null) {
            // Create a buffered image using the default color model
            int type = BufferedImage.TYPE_INT_RGB;
            if (hasAlpha) {
                type = BufferedImage.TYPE_INT_ARGB;
            }
            bimage = new BufferedImage(image.getWidth(null), image.getHeight(null), type);
        }
    
        // Copy image to buffered image
        Graphics g = bimage.createGraphics();
    
        // Paint the image onto the buffered image
        if ( flipped ) {
        	g.drawImage (image, 
                    0, image.getHeight(null), image.getWidth(null), 0,
                    0, 0, image.getWidth(null), image.getHeight(null),
                    null);	
        } else {
        	g.drawImage(image, 0, 0, null);
        }
        
        g.dispose();
    
        return bimage;
    }
    
    public static boolean hasAlpha(Image image) {
        // If buffered image, the color model is readily available
        if (image instanceof BufferedImage) {
            BufferedImage bimage = (BufferedImage)image;
            return bimage.getColorModel().hasAlpha();
        }
    
        // Use a pixel grabber to retrieve the image's color model;
        // grabbing a single pixel is usually sufficient
         PixelGrabber pg = new PixelGrabber(image, 0, 0, 1, 1, false);
        try {
            pg.grabPixels();
        } catch (InterruptedException e) {
        }
    
        // Get the image's color model
        ColorModel cm = pg.getColorModel();
        return cm.hasAlpha();
    }
    
    public static void launchInAFrame(String title, JComponent comp) {
        //1. Create the frame.
        JFrame frame = new JFrame(title);

        //2. Optional: What happens when the frame closes?
        try {
        	frame.addWindowListener((WindowListener)comp);
        } catch (ClassCastException e){
        }
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        //3. Create components and put them in the frame.
        //...create emptyLabel...
        frame.setContentPane(comp);

        //4. Size the frame.
        frame.pack();

        //5. Show it.
        frame.setVisible(true);
    }

    void setScale(boolean val) { m_scaleImage = val; }
    
    String m_lastOpenedDirectory;
    JFrame m_view;
    private ImagePanel m_imageCanvas;
    private EmptyEngine m_engine;
    private PopulationPanel m_popPanel;
    private JTextField m_portTextField;
    private CameraPanel m_camPanel;
    private ServerControlPanel m_serverPanel;
    private ServerVizUI m_self;
    private JCheckBox m_checkBox;
    private boolean m_scaleImage;
}
