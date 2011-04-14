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

import heaven.piece.{GoFish, Boxy}
import org.friendularity.{TestOgreComplexAnimDeluxeEdition, WomanFaceTest, JFugueTest}

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

		val box1 = boxLunch("1");
		tnc.addBoxToRoot(box1, false);
		tnc.launchFrame("FriendScale");
		WomanFaceTest.main(args);
		val box2 = boxLunch("2");
		tnc.addBoxToRoot(box2, false);
		JFugueTest.main(args);
	}
	class FriendBox extends Boxy.BoxOne {}
	class FriendTrig extends Boxy.TriggerOne {
		override def fire(box : Boxy.BoxOne) : Unit = {
			println(this.toString() + " friendly-firing on " + box.toString());
		}
	}
	def boxLunch(suffix : String) : FriendBox = {
		val fb1 = new FriendBox();
		fb1.setShortLabel("friendBox-" + suffix)
		val ft1 = new FriendTrig();
		ft1.setShortLabel("friendTrig-"  + suffix);
		fb1.attachTrigger(ft1);
		fb1;
	}
}
