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
package org.friendularity.gaze.bind.mio;

import java.awt.Rectangle;
import java.util.List;

import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.friendularity.gaze.util.GazeTracker;
import org.cogchar.sight.api.core.SightObservation;
import org.cogchar.api.animoid.gaze.IGazeTarget;
import org.osgi.framework.BundleContext;
import org.mechio.api.motion.Robot;
import org.mechio.api.motion.Robot.RobotPositionMap;
import org.mechio.api.motion.blending.FrameSource;
import org.mechio.api.motion.protocol.DefaultMotionFrame;
import org.mechio.api.motion.protocol.MotionFrame;
import org.mechio.api.motion.utils.RobotUtils;
import org.mechio.api.vision.ImageRegion;

/**
 *
 */
public class ImageTrackerFrameSource implements FrameSource<RobotPositionMap> {
    private ImageEgocentricConverter myCoordinateConverter;
    private GazeTracker myTracker;
    private GazeTargetMotionPlanner myPlanner;
    private BundleContext myContext;
    private Robot.Id myRobotId;
    
    public void trackRegion(ImageRegion region, long time){
        ImageJointSnapshotCoordinate ijsc = convertImageRegionToIJSC(region);
        if(ijsc == null){
            return;
        }
        SightObservation obs = convertIJSCtoObservation(ijsc);
        if(obs == null){
            return;
        }
        obs.setTimeStampMsec(time);
        myTracker.addObservation(obs);
    }
    
    private ImageJointSnapshotCoordinate convertImageRegionToIJSC(ImageRegion reg){
        RobotPositionMap pos = RobotUtils.getCurrentPositions(myContext, myRobotId);
        if(pos == null){
            return null;
        }
        int width = 320;
        int height = 240;
        return new ImageJointSnapshotCoordinate(width, height, reg, pos);
    }
    
    private SightObservation convertIJSCtoObservation(ImageJointSnapshotCoordinate ijsc){
        EgocentricDirection edir = myCoordinateConverter.convert(ijsc);
        SightObservation obs = new SightObservation();
        obs.setCenterDirection(edir);
        ImageRegion region = ijsc.getImageRegion();
        int x = region.getX();
        int y = region.getY();
        int w = region.getWidth();
        int h = region.getHeight();
        Rectangle rect = new Rectangle(x,y,w,h);
        obs.setBoundRect(rect);
        return obs;
    }

    @Override
    public MotionFrame getMovements(long time, long interval) {
        List<IGazeTarget> targets = myTracker.getObservationTrackers();
        if(targets == null || targets.isEmpty()){
            return null;
        }
        IGazeTarget target = targets.get(0);
        RobotPositionMap curPos = RobotUtils.getCurrentPositions(myContext, myRobotId);
        RobotPositionMap goals = myPlanner.getMovements(time, interval, target, curPos);
        
        MotionFrame frame = new DefaultMotionFrame();
        frame.setFrameLengthMillisec(interval);
        frame.setTimestampMillisecUTC(time);
        frame.setPreviousPositions(curPos);
        
        frame.setGoalPositions(goals);
        return frame;
    }
}
