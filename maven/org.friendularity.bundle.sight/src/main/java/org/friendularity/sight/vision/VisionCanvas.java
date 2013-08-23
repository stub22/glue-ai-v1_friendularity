/*
 * VisionCanvas.java
 * 
 * Created on Jul 21, 2007, 7:34:11 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import java.awt.Graphics;

/**
 *
 * @author josh
 */
    public class VisionCanvas extends java.awt.Canvas {
    private RawVisionObserver m_picDraw;
    
    public void SetObserver(RawVisionObserver obs)
    {
        m_picDraw = obs;
    }
    
    public void update(Graphics g) {
        paint(g);
    }
    
    public void paint(Graphics g) {
        //super.paint(g);
        if ( m_picDraw != null ) {
            m_picDraw.DrawVideo(g);
        }
    }
}
