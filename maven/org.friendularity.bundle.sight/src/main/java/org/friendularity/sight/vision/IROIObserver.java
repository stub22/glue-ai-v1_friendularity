/*
 * ROIObserver.java
 * 
 * Created on Jul 24, 2007, 3:38:30 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

/**
 *
 * @author josh
 */
public interface IROIObserver {
    public void ProcessFrame(int[] rectData);
}
