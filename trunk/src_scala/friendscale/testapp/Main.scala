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
		println("friendscale.testapp.main() START with args: " + args);
		val tnc = GoFish.makeTNC(args);

		// val box1 = Bony.boxLunch("blp1", "bls1");
		// tnc.addBoxToRoot(box1, false);
		tnc.launchFrame("FriendScale");
		val owTst = new Warbler(); // WomanFaceTest();
		owTst.startCanvasInPanelInFrame();
		val animCtrlsJL : java.util.List[AnimControl] = owTst.getAnimControls();
		println("Got animControls: " + animCtrlsJL);
		val animCtrlsSS : Seq[AnimControl] = scala.collection.JavaConversions.asScalaBuffer(animCtrlsJL) ;


		val rootBox = tnc.myBC.getRootBox();
		println("****************** Attaching scene box tree");
		val sceneBoxTree : Bony.FriendBox = Bony.attachSceneBoxTree(tnc.myBC, rootBox, animCtrlsSS);

		val scoreBoard = owTst.getScoreBoard();
		Bony.attachNudgerPair(tnc.myBC, sceneBoxTree, "pitch", 0.05f, scoreBoard);
		Bony.attachNudgerPair(tnc.myBC, sceneBoxTree, "yaw", 0.08f, scoreBoard);
		Bony.attachNudgerPair(tnc.myBC, sceneBoxTree, "roll", 0.12f, scoreBoard);

		Bony.attachNudgerPair(tnc.myBC, sceneBoxTree, "x", 0.5f, scoreBoard);
		Bony.attachNudgerPair(tnc.myBC, sceneBoxTree, "y", 0.7f, scoreBoard);
		Bony.attachNudgerPair(tnc.myBC, sceneBoxTree, "z", 0.6f, scoreBoard);


		println("****************** Finished attaching scene box tree");
		println("friendscale.testapp.main() END with args: " + args);
		
		// val box2 = Bony.boxLunch("blp2", "bls2");
		// tnc.addBoxToRoot(box2, false);

		// JFugueTest.main(args);
	}
	//
	trait Scope {
		
	}
	class ScopeBox   extends Boxy.BoxOne {

	}
	class Warbler() extends WomanFaceTest() {
		override def runUpdate(tpf : Float) : Unit = {
			// println("Warbler is Gargling!");
			super.runUpdate(tpf);
		}
	}
}
