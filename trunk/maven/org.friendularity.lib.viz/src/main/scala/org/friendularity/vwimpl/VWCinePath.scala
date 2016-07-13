/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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
package org.friendularity.vwimpl

import com.jme3.animation.LoopMode
import com.jme3.cinematic.MotionPath
import com.jme3.cinematic.events.MotionEvent
import com.jme3.scene.Spatial
import org.appdapter.core.name.Ident

/**
  * Created by Owner on 7/2/2016.
  *
  * Just a placeholder showing the JME Cine MotionPath API, which is an alternative form of gross
  * spatial anim control.    (Here the JME AnimControl is actually of class JME "MotionEvent".)
  *
  * TSee more notes and
  */


trait Unused_CinePathAnimable extends Movable with Locatable with Addressable {
	def getMoPath : MotionPath
	def applyPath(): Unit = {
		val moPath = getMoPath
		val spat = getMainSpat
		val initDur = 2.0f
		applyCinePath(spat, moPath, initDur)
	}
	def applyCinePath(aSpat : Spatial, mPath : MotionPath, initDur : Float) {
		val motEvCtrl = new MotionEvent(aSpat, mPath);
		motEvCtrl.setSpeed(1f);
		motEvCtrl.setInitialDuration(initDur);
		motEvCtrl.setLoopMode(LoopMode.DontLoop);
		aSpat.addControl(motEvCtrl);
		motEvCtrl.setEnabled(true);
	}
}
// TODO: Plug in whatever source of waypoints we like
class Unused_VWCinePathApplied(mySpat : Spatial) extends Unused_CinePathAnimable {

	val myMoPath = new MotionPath();
	myMoPath.addWayPoint(mySpat.getLocalTranslation());
	// myMPath.addWayPoint(newPos);
	myMoPath.setCurveTension(0.5f);

	override def getMoPath : MotionPath = myMoPath

	override def getID: Ident = ???

	override def getMainSpat: Spatial = mySpat
}

// Here's some apparent clarity, from 2014, on AnimFactory vs. MotionPath, MotionEvent
//
// https://hub.jmonkeyengine.org/t/playing-motionevent-and-animationcontrol-at-the-same-time-with-a-cinematic/30709/2

// Note Nehon's suggestion that MotionEvent be added as a control, as follows:
//		peerNode.addControl(motionEvent);
//		motionEvent.setEnabled(true);

/*   Here is the code to understand how a MotionEvent works as of 2016, although currently FriendU
  uses a 2014 snapshot.  We presume the code is close enough for review purps.

Note inner scene onUpdate is separate from control-update, and both are impl on MotionEvent:

From JME Cinematic MotionEvent.java:
Copyright (c) 2009-2016 jMonkeyEngine
public class MotionEvent extends AbstractCinematicEvent implements Control, JmeCloneable {

    protected Vector3f direction = new Vector3f();
    protected Vector3f lookAt = null;
    protected Vector3f upVector = Vector3f.UNIT_Y;
    protected Quaternion rotation = null;
    protected Direction directionType = Direction.None;

// The dir compute is in this method, which is called from the control-update method,
// also shown below

 public void onUpdate(float tpf) {
        traveledDistance = path.interpolatePath(time, this, tpf);
        computeTargetDirection();
    }
private void computeTargetDirection() {
        switch (directionType) {
            case Path:
                Quaternion q = new Quaternion();
                q.lookAt(direction, upVector);
                spatial.setLocalRotation(q);
                break;
            case LookAt:
                if (lookAt != null) {
                    spatial.lookAt(lookAt, upVector);
                }
                break;
            case PathAndRotation:
                if (rotation != null) {
                    Quaternion q2 = new Quaternion();
                    q2.lookAt(direction, upVector);
                    q2.multLocal(rotation);
                    spatial.setLocalRotation(q2);
                }
                break;
            case Rotation:
                if (rotation != null) {
                    spatial.setLocalRotation(rotation);

//  JME - control update
public void update(float tpf) {
        if (isControl) {
            internalUpdate(tpf);
        }
    }

    @Override
    public void internalUpdate(float tpf) {
        if (playState == PlayState.Playing) {
            time = time + (tpf * speed);
            if (loopMode == LoopMode.Loop && time < 0) {
                time = initialDuration;
            }
            if ((time >= initialDuration || time < 0) && loopMode == LoopMode.DontLoop) {
                if (time >= initialDuration) {
                    path.triggerWayPointReach(path.getNbWayPoints() - 1, this);
                }
                stop();
            } else {
                time = AnimationUtils.clampWrapTime(time, initialDuration, loopMode);
                if(time<0){
                    speed = - speed;
                    time = - time;
                }
                onUpdate(tpf);
            }
        }
    }
 */
