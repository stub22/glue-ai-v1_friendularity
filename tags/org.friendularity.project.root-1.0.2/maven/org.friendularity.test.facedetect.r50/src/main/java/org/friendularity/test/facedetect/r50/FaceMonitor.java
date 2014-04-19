/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.test.facedetect.r50;

import java.util.List;
import org.jflux.api.common.rk.position.NormalizedDouble;
import org.jflux.api.common.rk.utils.TimeUtils;
import org.jflux.api.core.Listener;
import org.mechio.api.motion.Joint;
import org.mechio.api.motion.Robot.JointId;
import org.mechio.api.motion.Robot.RobotPositionMap;
import org.mechio.api.motion.messaging.RemoteRobot;
import org.mechio.api.vision.ImageRegion;
import org.mechio.api.vision.ImageRegionList;
import org.mechio.client.basic.MechIO;
import org.mechio.client.basic.R50RobotJoints;
import org.mechio.client.basic.UserSettings;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class FaceMonitor implements Listener<ImageRegionList> {
    private final static int CAMERA_FRAME_WIDTH = 320;
    private final static int CAMERA_FRAME_HEIGHT = 240;
    private final static double MOVE_AMT = 0.01;
    
    private RemoteRobot myRobot;
    private double myYaw;
    private double myPitch;
    private JointId myNeckYaw;
    private JointId myNeckPitch;
    
    public FaceMonitor() {
        myRobot = MechIO.connectRobot();
        RobotPositionMap posMap = myRobot.getDefaultPositions();
        myNeckYaw = new JointId(myRobot.getRobotId(),
                new Joint.Id(R50RobotJoints.NECK_YAW));
        myNeckPitch = new JointId(myRobot.getRobotId(),
                new Joint.Id(R50RobotJoints.NECK_PITCH));
        
        myRobot.move(posMap, 1000);
        
        TimeUtils.sleep(1000);
        
        myYaw = posMap.get(myNeckYaw).getValue();
        myPitch = posMap.get(myNeckPitch).getValue();
    }
    
    public FaceMonitor(String ipAddress) {
        UserSettings.setRobotAddress(ipAddress);
        UserSettings.setAnimationAddress(ipAddress);
        UserSettings.setSpeechAddress(ipAddress);
        UserSettings.setRobotId("myRobot");
        myRobot = MechIO.connectRobot();
        RobotPositionMap posMap = myRobot.getDefaultPositions();
        myNeckYaw = new JointId(myRobot.getRobotId(),
                new Joint.Id(R50RobotJoints.NECK_YAW));
        myNeckPitch = new JointId(myRobot.getRobotId(),
                new Joint.Id(R50RobotJoints.NECK_PITCH));
        
        myRobot.move(posMap, 1000);
        
        TimeUtils.sleep(1000);
        
        myYaw = posMap.get(myNeckYaw).getValue();
        myPitch = posMap.get(myNeckPitch).getValue();
    }

    @Override
    public void handleEvent(ImageRegionList t) {
        List<ImageRegion> regions = t.getRegions();
        
        for(ImageRegion region: regions) {
            RobotPositionMap goals = myRobot.getGoalPositions();
            double centerX = CAMERA_FRAME_WIDTH / 2;
            double centerY = CAMERA_FRAME_HEIGHT / 2;
            double x = region.getX() + region.getWidth() / 2;
            double y = region.getY() + region.getHeight() / 2;

            if(x > centerX) {
//                double xDiff = x - centerX;
//                System.out.println("X is " + xDiff + " past center.");
                
                if(myYaw - MOVE_AMT > 0) {
                    myYaw -= MOVE_AMT;
//                    System.out.println("Moving yaw to " + myYaw);
                } else {
                    myYaw = 0;
                }
                
                goals.put(myNeckYaw, new NormalizedDouble(myYaw));
            } else if(x < centerX) {
//                double xDiff = centerX - x;
//                System.out.println("X is " + xDiff + " before center.");
                
                if(myYaw + MOVE_AMT < 1) {
                    myYaw += MOVE_AMT;
//                    System.out.println("Moving yaw to " + myYaw);
                } else {
                    myYaw = 1;
                }
                
                goals.put(myNeckYaw, new NormalizedDouble(myYaw));
            }
            
            if(y > centerY) {
//                double yDiff = y - centerY;
//                System.out.println("Y is " + yDiff + " past center.");
                
                if(myPitch - MOVE_AMT > 0) {
                    myPitch -= MOVE_AMT;
//                    System.out.println("Moving pitch to " + myPitch);
                } else {
                    myPitch = 0;
                }
                
                goals.put(myNeckPitch, new NormalizedDouble(myPitch));
            } else if(y < centerY) {
//                double yDiff = centerY - y;
//                System.out.println("Y is " + yDiff + " before center.");
                
                if(myPitch + MOVE_AMT < 1) {    
                    myPitch += MOVE_AMT;
//                    System.out.println("Moving pitch to " + myPitch);
                } else {
                    myPitch = 1;
                }
                
                goals.put(myNeckPitch, new NormalizedDouble(myPitch));
            }
            
            myRobot.move(goals, 100);
        }
    }
    
}
