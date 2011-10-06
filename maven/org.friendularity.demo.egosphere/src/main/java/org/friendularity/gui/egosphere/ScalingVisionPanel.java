/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.egosphere;

import java.awt.Graphics;
import java.awt.Image;
import java.util.List;
import java.util.logging.Logger;

import org.cogchar.vision.IAnnotatingObserver;
import org.cogchar.vision.IRawFrameObserver;
import org.cogchar.vision.RawFrameProcessor;

/**
 *
 * @author humankind
 */
public class ScalingVisionPanel extends javax.swing.JPanel {
    private static Logger	theLogger = Logger.getLogger(ScalingVisionPanel.class.getName());
	private IRawFrameObserver	myImageSource;
	
	public void setImageSource(IRawFrameObserver src) {
        myImageSource = src;
    }
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
        if ( myImageSource != null ) {
			drawImage(g);
        }
    }
	
	public void drawImage(Graphics g){
		Image img = myImageSource.getImage();
		if (img != null) {
            try {
                g.drawImage(img,  0, getHeight(), getWidth(), 0,
                            0, 0, img.getWidth(null), img.getHeight(null), null);
				List<IAnnotatingObserver> anns = ((RawFrameProcessor)myImageSource).getAnnotaters();
				for (IAnnotatingObserver iao : anns) {
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
	
}
