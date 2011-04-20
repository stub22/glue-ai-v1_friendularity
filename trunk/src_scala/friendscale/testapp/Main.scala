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

import heaven.piece.{GoFish, Boxy};
import com.jme3.animation.{AnimControl};
import org.friendularity.{TestOgreComplexAnimDeluxeEdition, WomanFaceTest, JFugueTest};

/**
 * @author Stu B. <www.texpedient.com>
 */

object Main {

  /**
   * @param args the command line arguments
   */
	def main(args: Array[String]): Unit = {
		println("friendscale.testapp.main() sez: Hello, world!");
		val tnc = GoFish.makeTNC(args);

		// val box1 = Bony.boxLunch("blp1", "bls1");
		// tnc.addBoxToRoot(box1, false);
		tnc.launchFrame("FriendScale");
		val owTst = new WomanFaceTest();
		owTst.startCanvasInPanelInFrame();
		val animCtrlsJL : java.util.List[AnimControl] = owTst.getAnimControls();
		println("Got animControls: " + animCtrlsJL);
		val animCtrlsSS : Seq[AnimControl] = scala.collection.JavaConversions.asScalaBuffer(animCtrlsJL) ;


		val rootBox = tnc.myBC.getRootBox();
		println("****************** Attaching scene box tree");
		val sceneBoxTree : Bony.FriendBox = Bony.attachSceneBoxTree(tnc.myBC, rootBox, animCtrlsSS);
		Bony.attachNudger(tnc.myBC, sceneBoxTree, "pitch", 0.05f);
		Bony.attachNudger(tnc.myBC, sceneBoxTree, "pitch", -0.05f);
		Bony.attachNudger(tnc.myBC, sceneBoxTree, "yaw", 0.05f);
		Bony.attachNudger(tnc.myBC, sceneBoxTree, "yaw", -0.05f);
		Bony.attachNudger(tnc.myBC, sceneBoxTree, "roll", 0.05f);
		Bony.attachNudger(tnc.myBC, sceneBoxTree, "roll", -0.05f);

		println("****************** Finished attaching scene box tree");
		
		// val box2 = Bony.boxLunch("blp2", "bls2");
		// tnc.addBoxToRoot(box2, false);

		// JFugueTest.main(args);
	}

}
