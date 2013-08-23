/*
 * ROIObserver.java
 * 
 * Created on Jul 24, 2007, 3:38:30 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import java.awt.Image;
import java.awt.Graphics;

/**
 *
 * @author Stu - based on RawVisionObserver
 */
public interface IRawFrameObserver {
    // The byte array form is used presently
    public void ProcessFrame(byte[] data);

    // The long-arg (pointer?) variant is not used presently by the C++ code.
    public void ProcessFrame(long addr);
    
    public Image getImage();
    public void DrawVideo(Graphics g);
            
    public void AddAnnotater(IAnnotatingObserver a);
    
    public void RemoveAnnotater(IAnnotatingObserver a);
}