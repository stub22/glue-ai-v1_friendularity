/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package friendscale.testapp

import heaven.piece.{GoFish, Boxy}
import org.friendularity.{TestOgreComplexAnimDeluxeEdition, WomanFaceTest, JFugueTest}
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
