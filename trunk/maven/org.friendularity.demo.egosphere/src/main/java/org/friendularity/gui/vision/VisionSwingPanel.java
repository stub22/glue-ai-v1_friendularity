/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.vision;

import java.awt.Graphics;
import org.cogchar.vision.IRawFrameObserver;

/**
 *
 * @author StuB
 */
public class VisionSwingPanel extends javax.swing.JPanel  {
    private IRawFrameObserver  myImageSource;
    
	public void setImageSource(IRawFrameObserver src) {
        myImageSource = src;
    }
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
        if ( myImageSource != null ) {
            myImageSource.DrawVideo(g);
        }
    }    
}
