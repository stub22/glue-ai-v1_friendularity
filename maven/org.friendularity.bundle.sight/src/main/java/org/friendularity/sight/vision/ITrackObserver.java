/*
 * ITrackObserver.java
 * 
 * Created on Jul 30, 2007, 12:23:33 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

/**
 *
 * @author josh
 */
public interface ITrackObserver {
    public void ProcessFrame(int[][] trackData);
}
