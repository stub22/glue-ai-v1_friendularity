/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.gaze.bind.rk;

import org.robokind.api.motion.Robot.RobotPositionMap;
import org.robokind.api.vision.ImageRegion;

/**
 *
 * @author Matthew Stevenson
 */
public class ImageJointSnapshotCoordinate {
    private int myImageWidth;
    private int myImageHeight;
    private ImageRegion myRegion;
    private RobotPositionMap myJointPositions;
    
    public ImageJointSnapshotCoordinate(
            int width, int height, ImageRegion reg, RobotPositionMap pos){
        myImageWidth = width;
        myImageHeight = height;
        myRegion = reg;
        myJointPositions = pos;
    }
    
    public int getImageWidth(){
        return myImageWidth;
    }
    public int getImageHeight(){
        return myImageHeight;
    }
    public ImageRegion getImageRegion(){
        return myRegion;
    }
    public RobotPositionMap getJointPositions(){
        return myJointPositions;
    }
}
