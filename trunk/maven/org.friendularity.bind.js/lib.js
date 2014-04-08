importPackage(org.mechio.client.basic);
importPackage(org.mechio.api.motion);
importPackage(org.jflux.api.common.rk.position);
//importPackage(org.jflux.api.common.rk.utils);

function checkExit() {
    var b = controlSystem.isExitingNow();
    if(controlSystem.isExitingNow()) {
        throw new Error('now exiting...');
    }
}

function SRobot(robot) {
    checkExit();
    var myRobot = robot;

    this.move = function(positions, lenMillisec) {
        checkExit();
        myRobot.move(positions, lenMillisec);
    }

    this.getRobotId = function() {
        checkExit();
        return myRobot.getRobotId();
    }
}

function SAnimPlayer(animPlayer) {
    checkExit();
    var myAnimPlayer = animPlayer;

    this.playAnimation = function(animation) {
        checkExit();
        return myAnimPlayer.playAnimation(animation);
    }

    this.loopAnimation = function(animation) {
        checkExit();
        return myAnimPlayer.loopAnimation(animation);
    }

    this.stopAnimation = function(animation) {
        myAnimPlayer.stopAnimation(animation);
        checkExit();
    }
}

function connectRobot(ipAddress) {
    checkExit();

    if(ipAddress === undefined) {
        return new SRobot(MechIO.connectRobot());
    } else {
        return new SRobot(MechIO.connectRobot(ipAddress));
    }
}

function connectAnimationPlayer(ipAddress) {
    checkExit();

    if(ipAddress === undefined) {
        return new SAnimPlayer(MechIO.connectAnimationPlayer());
    } else {
        return new SAnimPlayer(MechIO.connectAnimationPlayer(ipAddress));
    }
}
