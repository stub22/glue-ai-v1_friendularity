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
import org.friendularity.{ScoreBoard};
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
		def nudgeBone(direction : String,  displacement : Float, scoreBoard : ScoreBoard) : Unit = {
			val localPos : Vector3f = myBone.getLocalPosition();
			val localRotQ : Quaternion = myBone.getLocalRotation();
			val localScale : Vector3f = myBone.getLocalScale();

			// "Model" values are same as the "World" values.
			// When a bone has no parent, these values will be copies of the "local" vals.
			// When there is a parent, then these model values reflect the local offest
			// to the parent model values.
			//
			val modelPos : Vector3f = myBone.getModelSpacePosition();
			val modelRotQ : Quaternion = myBone.getModelSpaceRotation();
			val modelScale : Vector3f = myBone.getModelSpaceScale();
			println("=================================================================");
			println("old model pos=" + modelPos);
			println("old model rot=" + modelRotQ);
			println("old model scale=" + modelScale);

			val initialPos : Vector3f = myBone.getInitialPos();
			val initialRotQ : Quaternion = myBone.getInitialRot();
			// val initialScale : Vector3f = myBone.getInitialScale();  no getter...
			println("=================================================================");
			println("old initialPos=" + initialPos);
			println("old localPos=" + localPos);
			println("old initialRotQ=" + initialRotQ);
			println("old localRotQ=" + localRotQ);	
			println("old localScale=" + localScale);
			println("=================================================================");
			// Vector3f modelPos = rootBone.getModelSpacePosition();

			val nudgedRotQ = new Quaternion();
			// yaw, roll, pitch
			//	System.out.println("Setting roll for bone: " + b + " to " + myWaistTwistAngle);

			var pitchAngle = 0.0f;
			var rollAngle = 0.0f;
			var yawAngle = 0.0f;

			var xDisp = 0.0f;
			var yDisp = 0.0f;
			var zDisp = 0.0f;

			if (direction.equals("pitch")) {
				pitchAngle = displacement;
			} else if (direction.equals("roll")) {
				rollAngle = displacement;
			} else if (direction.equals("yaw")) {
				yawAngle = displacement;
			} else if (direction.equals("x")) {
				xDisp = displacement;
			} else if (direction.equals("y")) {
				yDisp = displacement;
			} else if (direction.equals("z")) {
				zDisp = displacement;
			} else {
				println("Unknown nudge direction: " + direction);
			}
			nudgedRotQ.fromAngles(pitchAngle, rollAngle, yawAngle);
			val nextRotQ = localRotQ.mult(nudgedRotQ);
			val displacePosVec = new Vector3f(xDisp, yDisp, zDisp);
			val nextPosVec = initialPos.add(displacePosVec);

			scoreBoard.displayScore(0, "bone: " + myBone);
			scoreBoard.displayScore(1, "posVec: " + nextPosVec);
			scoreBoard.displayScore(2, "rotQ: " + nextRotQ);

			println("next posVec: " + nextPosVec);
			println("next rotQ=" + nextRotQ);
			println("=================================================================");
			// Note that "userControl" makes the following methods silentlyl disabled:
			//		setAnimTransforms
			//		blendAnimTransforms
			//		reset
			// myBone.setUserControl(true);
		
			// Vector3f.ZERO
			// Note that scale is set to 0.5f during initial placement
			

			myBone.setBindTransforms(nextPosVec, nextRotQ, Vector3f.UNIT_XYZ);
			//When using "userTransforms"
			// These are all applied as DELTAS from the initialPos, initialRot, initialScale,
			// to yield a new value of localPos, localRot, localScale.
			//
			//  essentially:       localPos = initialPos + translation
			//						localRot = initalRot * rotation
			//						localScale = initialScale * scale

			// myBone.setUserTransforms(displacePosVec, nudgedRotQ, Vector3f.UNIT_XYZ);
			// myBone.setBindingPose();   is protected scope.
			// setBindingPose will write the current localP,R,S into initialP,R,S
			// and recursively do so on all child bones.
		}
	}
	class BoneTrig(shortLabel: String) extends FriendTrig(shortLabel) {
		setShortLabel(shortLabel);
		override def fire(box : Boxy.BoxOne) : Unit = {
			println(this.toString() + " bone-thugging on " + box.toString());
		}
	}
	class NudgeTrig(val myDir : String, val myAmt : Float, val myScoreBoard : ScoreBoard) extends BoneTrig("nudge-" + myDir + " " + myAmt) {
		override def fire(box : Boxy.BoxOne) : Unit = {
			val boneBox = box.asInstanceOf[BoneBox];
			println(this.toString() + " nudging " + box.toString());
			boneBox.nudgeBone(myDir, myAmt, myScoreBoard);
		}
	}
	def attachNudger(bc: BoxContext, boxTree : FriendBox, dir : String, amt : Float, scoreBoard : ScoreBoard) : Unit = {
		val nt = new Bony.NudgeTrig(dir, amt, scoreBoard);
		boxTree.attachTrigToKids(bc, nt, true);
	}
	def attachNudgerPair(bc: BoxContext, boxTree : FriendBox, dir : String, amt : Float, scoreBoard : ScoreBoard) : Unit = {
		attachNudger(bc, boxTree, dir, amt, scoreBoard);
		attachNudger(bc, boxTree, dir, -1.0f * amt, scoreBoard);
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
