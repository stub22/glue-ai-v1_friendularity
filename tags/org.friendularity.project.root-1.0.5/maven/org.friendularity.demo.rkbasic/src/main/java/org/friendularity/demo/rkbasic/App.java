package org.friendularity.demo.rkbasic;

import org.jflux.api.common.rk.position.NormalizedDouble;
import org.mechio.api.animation.Animation;
import org.mechio.api.animation.messaging.RemoteAnimationPlayerClient;
import org.mechio.api.animation.player.AnimationJob;
import org.mechio.api.motion.Joint;
import org.mechio.api.motion.messaging.RemoteRobot;
import org.mechio.api.speech.messaging.RemoteSpeechServiceClient;
import org.mechio.client.basic.MechIO;
import org.mechio.client.basic.UserSettings;

import static org.mechio.api.motion.Robot.*;
import static org.mechio.client.basic.R50RobotJoints.*;

public class App {
    private static RemoteRobot myRobot;
    private static RemoteAnimationPlayerClient myPlayer;
    private static RemoteSpeechServiceClient mySpeaker;
    private static RobotPositionMap myGoalPositions;
    
    public static void main( String[] args ){
        long animLen;
        
        // If running on a robot instead of an avatar:
        //     uncomment the next four lines and change the IP to the robot's IP
        //UserSettings.setRobotAddress("127.0.0.1");
        //UserSettings.setAnimationAddress("127.0.0.1");
        //UserSettings.setSpeechAddress("127.0.0.1");
        //UserSettings.setRobotId("myRobot");
        
        myRobot = MechIO.connectRobot();
        myPlayer = MechIO.connectAnimationPlayer();
        mySpeaker = MechIO.connectSpeechService();
        
        // This will move the robot or avatar's waist and arm.
        // If running on a robot instead of an avatar:
        //     MAKE SURE IT'S NOT IN A STAND
        //     SERIOUSLY, DO NOT DO THIS IF THE ROBOT'S MOVEMENTS ARE RESTRICTED
        JointId waist = new JointId(myRobot.getRobotId(), new Joint.Id(WAIST));
        JointId arm = new JointId(
                myRobot.getRobotId(), new Joint.Id(RIGHT_SHOULDER_ROLL));
        
        myGoalPositions = new RobotPositionHashMap();
        myGoalPositions.put(waist, new NormalizedDouble(1.0));
        myGoalPositions.put(arm, new NormalizedDouble(0.5));
        myRobot.move(myGoalPositions, 1000);
        
        Animation introAnim = MechIO.loadAnimation("intro.anim.xml");
        AnimationJob introJob = myPlayer.playAnimation(introAnim);
        animLen = introAnim.getLength();
        mySpeaker.speak("Hello, my name is ZENO.");
        MechIO.sleep(500 + animLen);
        
        MechIO.disconnect();
        System.exit(0);
    }
}