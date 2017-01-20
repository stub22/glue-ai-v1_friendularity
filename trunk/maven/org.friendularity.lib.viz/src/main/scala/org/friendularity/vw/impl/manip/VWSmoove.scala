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
package org.friendularity.vw.impl.manip

import com.jme3.animation.{AnimChannel, AnimControl => JmeAnimCtrl, AnimEventListener => JmeAnimEventListener, Animation => JmeGrossAnim}
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vw.mprt.manip.ManipCompletionHandle

trait OldFileOfCommentNotes

/**
  * Created by Stub22 on 6/30/2016.
  * Capturing the most important traits needed for smooth anim and transform of given spatials,
  * using JME features, primarily via the class com.jme3.animation.AnimationFactory.
  */

// trait VWSmoove

/*  Inefficient RAM use in AnimationFactory - we could do a tighter  MotionTrack creator than this:
https://github.com/jMonkeyEngine/jmonkeyengine/blob/master/jme3-core/src/main/java/com/jme3/animation/AnimationFactory.java
        totalFrames = (int) (fps * duration) + 1;
        tpf = 1 / (float) fps;
        times = new float[totalFrames];
        translations = new Vector3f[totalFrames];
        rotations = new Quaternion[totalFrames];
        scales = new Vector3f[totalFrames];
 */


// Other main tricks for rot of a spatial are shown in JME Cine MotionEvent.computeTargetDirection, such as
// directly instructing the spatial to look at a Vector3f lookAt, in *world* coordinates.    Reviewing the
// code indicates
// that it just sets the localRot once, does not establish a lower constraint.
// Also note that lookAt is in *world* coordinates.

// spatial.lookAt(lookAt, upVector);

// ...or instead creating a compound quaternion rotation to make spat to point at an offset from the lookAt.
//
// q2.lookAt(direction, upVector);
// q2.multLocal(rotation);
// Quaternion q2 = new Quaternion();
// spatial.setLocalRotation(q2);

// See:   https://github.com/jMonkeyEngine/jmonkeyengine/blob/master/jme3-core/src/main/java/com/jme3/cinematic/events/MotionEvent.java
