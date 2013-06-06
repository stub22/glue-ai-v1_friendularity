package rkbasic.demo;

import org.robokind.api.animation.Animation;
import org.robokind.api.animation.messaging.RemoteAnimationPlayerClient;
import org.robokind.api.animation.player.AnimationJob;
import org.robokind.api.common.position.NormalizedDouble;
import org.robokind.api.motion.Joint;
import org.robokind.api.motion.messaging.RemoteRobot;
import org.robokind.api.speech.messaging.RemoteSpeechServiceClient;
import org.robokind.client.basic.Robokind;

import static org.robokind.api.motion.Robot.*;
import static org.robokind.client.basic.RobotJoints.*;

public class App {
    private static RemoteRobot myRobot;
    private static RemoteAnimationPlayerClient myPlayer;
    private static RemoteSpeechServiceClient mySpeaker;
    private static RobotPositionMap myGoalPositions;
    
    public static void main( String[] args ){
        long animLen;
        
        myRobot = Robokind.connectRobot();
        myPlayer = Robokind.connectAnimationPlayer();
        mySpeaker = Robokind.connectSpeechService();
        
        JointId waist = new JointId(myRobot.getRobotId(), new Joint.Id(WAIST));
        JointId leg = new JointId(
                myRobot.getRobotId(), new Joint.Id(RIGHT_HIP_YAW));
        
        myGoalPositions = new RobotPositionHashMap();
        myGoalPositions.put(waist, new NormalizedDouble(1.0));
        myGoalPositions.put(leg, new NormalizedDouble(0.5));
        myRobot.move(myGoalPositions, 1000);
        
        Animation introAnim = Robokind.loadAnimation("intro.anim.xml");
        AnimationJob introJob = myPlayer.playAnimation(introAnim);
        animLen = introAnim.getLength();
        mySpeaker.speak("Hello, my name is ZENO.");
        Robokind.sleep(500 + animLen);
        
        Robokind.disconnect();
        System.exit(0);
    }
}