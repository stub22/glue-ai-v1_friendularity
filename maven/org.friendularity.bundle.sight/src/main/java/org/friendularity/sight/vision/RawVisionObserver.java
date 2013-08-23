/*
 * RawVisionObserver.java
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
import java.awt.geom.AffineTransform;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

/**
 *
 * @author josh
 */
public class RawVisionObserver extends ThreadAwareObject implements IRawFrameObserver {
    
    public RawVisionObserver(java.awt.Canvas p) {
		super();
        m_panel = p;
        Iterator readers = ImageIO.getImageReadersByFormatName("bmp");
        m_reader = (ImageReader)readers.next();
        m_annotaters = new ArrayList();
        m_transform = new AffineTransform();
        m_transform.scale(1.0, -1.0);
    }
    
    public void ProcessFrame(long addr) {
		blessCurrentThread();
        m_addr = addr;
        m_panel.repaint();
        //m_addr = 0;
    }
    
    public void ProcessFrame(byte[] data) {
		blessCurrentThread();
        m_data = data;
        m_panel.repaint();
        //m_data = null;
    }
    
    public void AddAnnotater(IAnnotatingObserver a) {
        m_annotaters.add(a);
    }
    
    public void RemoveAnnotater(IAnnotatingObserver a)
    {
        m_annotaters.remove(a);
    }
    
    public void DrawVideo(Graphics g) {
        if ( m_data != null ) {
            InputStream data = new ByteArrayInputStream(m_data);
            try {
                ImageInputStream iis = ImageIO.createImageInputStream(data);
                m_reader.setInput(iis, true);
                m_image = m_reader.read(0);
                g.drawImage(m_image,  0, m_image.getHeight(null), m_image.getWidth(null), 0,
                            0, 0, m_image.getWidth(null), m_image.getHeight(null), null);
                
                try {
                    for(int index=0; ; index++) {
                        ((IAnnotatingObserver)m_annotaters.get(index)).Annotate(g);
                    }
                } catch(ArrayIndexOutOfBoundsException aioobe) {
                    // ignore; this just means that i has reached the end of the array
                }
            } catch (Exception e) {
            }
        }
    }
    
    public Image getImage() {
    	return m_image;
    }
    
    private java.awt.Canvas m_panel;
    private long m_addr;
    private byte[] m_data;
    private ImageReader m_reader;
    private ArrayList m_annotaters;
    private AffineTransform m_transform;
    private Image m_image;
}
