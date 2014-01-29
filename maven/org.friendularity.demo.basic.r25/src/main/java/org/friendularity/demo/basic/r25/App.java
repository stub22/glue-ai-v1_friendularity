package org.friendularity.demo.basic.r25;

import org.robokind.api.animation.Animation;
import org.robokind.api.animation.messaging.RemoteAnimationPlayerClient;
import org.robokind.api.animation.player.AnimationJob;
import org.robokind.api.common.position.NormalizedDouble;
import org.robokind.api.common.utils.TimeUtils;
import org.robokind.api.motion.Joint;
import org.robokind.api.motion.messaging.RemoteRobot;
import org.robokind.api.speech.messaging.RemoteSpeechServiceClient;
import org.robokind.client.basic.Robokind;
import org.robokind.client.basic.UserSettings;

import static org.friendularity.demo.basic.r25.RobotJointsR25.*;
import static org.robokind.api.motion.Robot.*;

public class App {
    private static RemoteRobot myRobot;
    private static RemoteAnimationPlayerClient myPlayer;
    private static RemoteSpeechServiceClient mySpeaker;
    private static RobotPositionMap myGoalPositions;
    
    public static void main( String[] args ){
        long animLen;
        
        // If running on a robot instead of an avatar:
        //     uncomment the next five lines and change the IP to the robot's IP
        String ipAddress = "192.168.2.101";
        UserSettings.setRobotAddress(ipAddress);
        UserSettings.setSpeechAddress(ipAddress);
        UserSettings.setAnimationAddress(ipAddress);
        UserSettings.setRobotId("myRobot");
        
        myRobot = Robokind.connectRobot();
        myPlayer = Robokind.connectAnimationPlayer();
        mySpeaker = Robokind.connectSpeechService();
        
        // This will move the robot or avatar's waist and arm.
        // If running on a robot instead of an avatar:
        //     MAKE SURE IT'S NOT IN A STAND
        //     SERIOUSLY, DO NOT DO THIS IF THE ROBOT'S MOVEMENTS ARE RESTRICTED
        JointId waist = new JointId(myRobot.getRobotId(), new Joint.Id(WAIST));
        JointId arm = new JointId(
                myRobot.getRobotId(), new Joint.Id(LEFT_SHOULDER_ROLL));
        
        myGoalPositions = new RobotPositionHashMap();
        myGoalPositions.put(waist, new NormalizedDouble(1.0));
        myGoalPositions.put(arm, new NormalizedDouble(1.0));
        myRobot.move(myGoalPositions, 1000);
        
        TimeUtils.sleep(1000);
        
        myGoalPositions.put(waist, new NormalizedDouble(0.5));
        myGoalPositions.put(arm, new NormalizedDouble(0.0));
        myRobot.move(myGoalPositions, 1000);
        
        Animation introAnim =
                Robokind.loadAnimation("AZR25_waveHand_01.anim.xml");
        AnimationJob introJob = myPlayer.playAnimation(introAnim);
        animLen = introAnim.getLength();
        mySpeaker.speak("Hello, I am your new companion.");
        Robokind.sleep(500 + animLen);
        
        Robokind.disconnect();
        System.exit(0);
    }
}