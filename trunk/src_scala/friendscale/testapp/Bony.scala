/*
 *  Copyright 2011 by The Friendularity Project (www.friendularity.org).
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

package friendscale.testapp

import com.appdapter.gui.box.{Box, BoxContext};
import heaven.piece.{GoFish, Boxy};
import com.jme3.animation.{AnimChannel, AnimControl, Bone, LoopMode, Skeleton};
import com.jme3.math.{Quaternion, Vector3f};
/*
import com.jme3.app.SimpleApplication;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.Rectangle;
import com.jme3.light.DirectionalLight;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.renderer.Camera;
import com.jme3.system.AppSettings;
import com.jme3.system.JmeCanvasContext;
import com.jme3.system.JmeContext;
*/

/**
 * @author Stu B. <www.texpedient.com>
 */

object Bony {

	def attachSceneBoxTree(bc : BoxContext, parentBox : Box[_], animCtrls : Seq[AnimControl]) : FriendBox = {
		val sceneBox = new FriendBox("-scene[acCount=" + animCtrls.length + "]");
		bc.contextualizeAndAttachChildBox(parentBox, sceneBox);
		for (ac <- animCtrls) {
			val skel = ac.getSkeleton();
			println("Attaching skeleton for " + skel);
			val skelBB = attachSkeletonBoxTree(bc, sceneBox, skel);
			
		}
		sceneBox;
	}
	def attachSkeletonBoxTree(bc : BoxContext, parentBox : Box[_], skel : Skeleton) : FriendBox = {
		val skelBox =  new FriendBox("[" + skel + "]");
		bc.contextualizeAndAttachChildBox(parentBox, skelBox);
		val roots : Array[Bone] = skel.getRoots();
		for (rb <- roots) {
			attachBoneBoxTree(bc, skelBox, rb);
		}
		skelBox;
	}
	def attachBoneBoxTree(bc : BoxContext, parentBox : Box[_], bone : Bone) : BoneBox = {
		val boneBox =  new BoneBox(bone);
		bc.contextualizeAndAttachChildBox(parentBox, boneBox);
		val kidJL : java.util.List[Bone] = bone.getChildren();
		val kidSS : Seq[Bone] = scala.collection.JavaConversions.asScalaBuffer(kidJL) ;
		for (kid <- kidSS) {
			attachBoneBoxTree(bc, boneBox, kid);
		}
		boneBox;
	}

	/*
	


		// System.out.println("================================================================");
		myVCP.setDumpText("tgtBone=" + tgtBone + ", localPos=" + localPos + ", modelPos=" + modelPos);
	*/


	class FriendBox(shortLabel : String) extends Boxy.BoxOne {
		setShortLabel(shortLabel);
		def attachTrigToKids(bc : BoxContext, trig : FriendTrig, recursive : Boolean) : Unit = {
			// Assume for the moment that all kids are also friendBoxes.
			val kidBoxes = getOpenKidBoxes(bc);
			for (kid <- kidBoxes) {
				kid.attachTrigger(trig);
				if (recursive) {
					// TODO : add a match, or get fancier with types.
					val fk : FriendBox = kid.asInstanceOf[FriendBox];
					fk.attachTrigToKids(bc, trig, true);
				}
			}
		}
	}
	class FriendTrig(shortLabel: String) extends Boxy.TriggerOne {
		override def fire(box : Boxy.BoxOne) : Unit = {
			println(this.toString() + " friendly-firing on " + box.toString());
		}
	}
	class BoneBox(val myBone : Bone) extends FriendBox("[" + myBone.toString() + "]") {
		def nudgeBone(direction : String,  angleRad : Float) : Unit = {
			val localPos : Vector3f = myBone.getLocalPosition();
			val localRot : Quaternion = myBone.getLocalRotation();
			val localScale : Vector3f = myBone.getLocalScale();
			// Vector3f modelPos = rootBone.getModelSpacePosition();

			val q = new Quaternion();
			// yaw, roll, pitch
			//	System.out.println("Setting roll for bone: " + b + " to " + myWaistTwistAngle);

			var pitchAngle = 0.0f;
			var rollAngle = 0.0f;
			var yawAngle = 0.0f;
			if (direction.equals("pitch")) {
				pitchAngle = angleRad;
			} else if (direction.equals("roll")) {
				rollAngle = angleRad;
			} else if (direction.equals("yaw")) {
				yawAngle = angleRad;
			}
			q.fromAngles(pitchAngle, rollAngle, yawAngle);
			val nextRotQ = localRot.mult(q);
			myBone.setUserControl(true);
			myBone.setUserTransforms(Vector3f.ZERO, nextRotQ, Vector3f.UNIT_XYZ);
		}
	}
	class BoneTrig(shortLabel: String) extends FriendTrig(shortLabel) {
		setShortLabel(shortLabel);
		override def fire(box : Boxy.BoxOne) : Unit = {
			println(this.toString() + " bone-thugging on " + box.toString());
		}
	}
	class NudgeTrig(val myDir : String, val myAmt : Float) extends BoneTrig("nudge-" + myDir + "-" + myAmt) {
		override def fire(box : Boxy.BoxOne) : Unit = {
			val boneBox = box.asInstanceOf[BoneBox];
			println(this.toString() + " nudging " + box.toString());
			boneBox.nudgeBone(myDir, myAmt);
		}
	}
	def attachNudger(bc: BoxContext, boxTree : FriendBox, dir : String, amt : Float) : Unit = {
		val nt = new Bony.NudgeTrig(dir, amt);
		boxTree.attachTrigToKids(bc, nt, true);
	}
	/*
	def boxLunch(labelPrefix : String, labelSuffix : String) : BoneBox = {
		val bb1 = new BoneBox(labelPrefix + "-bba-" + labelSuffix);
		val bt1 = new BoneTrig(labelPrefix + "-bt-a.1-" + labelSuffix);
		bb1.attachTrigger(bt1);
		bb1;
	}
	*/
}
