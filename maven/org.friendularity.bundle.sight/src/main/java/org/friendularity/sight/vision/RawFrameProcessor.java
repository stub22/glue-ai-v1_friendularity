/*
 * RawFrameProcessor.java
 *
 * Created on Jul 20, 2007, 11:57:35 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import org.cogchar.zzz.oldboot.ThreadAwareObject;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;

import javax.imageio.stream.ImageInputStream;

/**
 * @author Stu - copied from RawVisionObserver
 */
public class RawFrameProcessor extends java.util.Observable implements IRawFrameObserver {
	private static Logger theLogger = Logger.getLogger(RawFrameProcessor.class.getName());

    private ThreadAwareObject myTAO;
    private long m_addr;
    private byte[] m_data;
    private ImageReader m_reader;
    private List<IAnnotatingObserver> m_annotaters;
    // private AffineTransform m_transform;
    private BufferedImage m_image;    
	
	int		frameCount = 0;
    
    public RawFrameProcessor() {
		myTAO = new ThreadAwareObject();
        Iterator readers = ImageIO.getImageReadersByFormatName("bmp");
        m_reader = (ImageReader)readers.next();
        m_annotaters = new ArrayList<IAnnotatingObserver>();
        // m_transform = new AffineTransform();
       //  m_transform.scale(1.0, -1.0);
    }

    //   This form will be called
	// Note that a BMP header was written onto this data in
	// JNIVisionFacade_RawVision.cpp-> writeBmpHeader()
    public synchronized void ProcessFrame(byte[] data) {
		myTAO.blessCurrentThread();
		frameCount++;
		long frameTime = System.currentTimeMillis();
		// theLogger.info("ProcessFrame got frame# " + frameCount + " at " + frameTime);

		// This assignment creates a local reference to the array, which will be
		// the ONLY reference as soon as the C++ code in   
		// RawVisionJavaObserver :: operator() releases its reference.
		// So, when this ref gets assigned-over next time, it can be garbage collected.
		// Seems to be working!
        m_data = data;
		m_image = null;
        setChanged();
        notifyObservers();        
    }
    
    public void AddAnnotater(IAnnotatingObserver a) {
        m_annotaters.add(a);
    }
    
    public void RemoveAnnotater(IAnnotatingObserver a) {
        m_annotaters.remove(a);
    }
	
	public List<IAnnotatingObserver> getAnnotaters(){
		return m_annotaters;
	}
    public void DrawVideo(Graphics g) {
		Image img = getImage();
		if (img != null) {
            try {
				// This draws a vertically inverted image!!!
                g.drawImage(img,  0, img.getHeight(null), img.getWidth(null), 0,
                            0, 0, img.getWidth(null), img.getHeight(null), null);
				for (IAnnotatingObserver iao : m_annotaters) {
					iao.Annotate(g);
				}
            } catch (Exception e) {
				System.out.println("RawFrameProcessor caught exception: " + e);
				e.printStackTrace();
            }
        } else {
			System.out.println("RawFrameProcessor is not drawing, because getImage() yields null");
		}
    }
    public synchronized BufferedImage getImage() {
		if ((m_image == null) && (m_data != null)) {
			InputStream data = new ByteArrayInputStream(m_data);
			try {
				
/* We occasionally get an exception like this: [Synchronization problem?]
 *		getImage() caught exception: java.io.EOFException
        at java.io.RandomAccessFile.readFully(RandomAccessFile.java:383)
        at javax.imageio.stream.FileCacheImageInputStream.read(FileCacheImageInputStream.java:167)
        at javax.imageio.stream.ImageInputStreamImpl.readFully(ImageInputStreamImpl.java:337)
        at com.sun.imageio.plugins.bmp.BMPImageReader.read24Bit(BMPImageReader.java:1162)
        at com.sun.imageio.plugins.bmp.BMPImageReader.read(BMPImageReader.java:817)
        at javax.imageio.ImageReader.read(ImageReader.java:923)
        at org.appdapter.vision.RawFrameProcessor.getImage(RawFrameProcessor.java:97)
        at org.appdapter.vision.RawFrameProcessor.getOCV_SubImage(RawFrameProcessor.java:107)
        at org.appdapter.app.vision.FaceModel.facesSeen(FaceModel.java:74)
        at org.appdapter.app.vision.FaceDetectNoticeProcessor.ProcessFrame(FaceDetectNoticeProcessor.java:35)
 */
				ImageInputStream iis = ImageIO.createImageInputStream(data);
				m_reader.setInput(iis, true);
				m_image = m_reader.read(0);
            } catch (Throwable t) {
				System.out.println("getImage() caught exception: " + t);
				t.printStackTrace();
            }
		}
    	return m_image;
    }    
    public PortableImage getPortableSubImage(Rectangle bbox, boolean flipVertical) {
		PortableImage pimg = null;
		BufferedImage img = getImage();
		if (img != null) {
			// BufferedImage subImage = img.getSubimage(bbox.x, bbox.y, bbox.width, bbox.height);
			pimg = new PortableImage(img, bbox, flipVertical);
		}
		return pimg;
	}
    //   This variant is disabled with "if ( 0 )" in the C code
    public void ProcessFrame(long addr) {
        System.out.println("********  UNEXPECTED ----- RawFrameProcessor got a LONG value: " + addr);
    }    

}
