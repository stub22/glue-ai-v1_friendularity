var myRobot = connectRobot();
var myPlayer = connectAnimationPlayer();
//var mySpeaker = MechIO.connectSpeechService();

var waist = new Robot.JointId(myRobot.getRobotId(), new Joint.Id(R25RobotJoints.WAIST));
var arm = new Robot.JointId(myRobot.getRobotId(), new Joint.Id(R25RobotJoints.LEFT_SHOULDER_ROLL));

var myGoalPositions = new Robot.RobotPositionHashMap();

myGoalPositions.put(waist, new NormalizedDouble(0.0));
myGoalPositions.put(arm, new NormalizedDouble(0.0));
myRobot.move(myGoalPositions, 1000);

MechIO.sleep(1000);

myGoalPositions.put(waist, new NormalizedDouble(0.5));
myGoalPositions.put(arm, new NormalizedDouble(0.5));
myRobot.move(myGoalPositions, 1000);

var introAnim = MechIO.loadAnimation("AZR25_waveHand_01.anim.xml");
var introJob = myPlayer.playAnimation(introAnim);
var animLen = introAnim.getLength();
MechIO.sleep(500 + animLen);

MechIO.disconnect();
