package org.friendularity.rbody

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.skeleton.config.{BoneRotationAxis, BoneProjectionPosition}
import org.cogchar.bind.mio.robot.model.{ModelRobot, ModelRobotUtils}
import org.cogchar.render.model.bony.{BoneState, FigureState}
import org.cogchar.render.model.humanoid.HumanoidFigure

import java.util.{Map => JMap, List => JList}
/**
  * Created by Stub22 on 9/6/2016.
  * Port of Cogchar code from ModelToFigureStateMappingFuncs.java and VWorldRoboPump.java.
  * This code is the primary intersection of o.c.mio(= mechio robot) with VWorld bones,
  * and thus has dependency implications as well as functional importance.
  */
trait RoboBoneStateMapping extends  VarargsLogging {
	def propagateState(br: ModelRobot, hf: HumanoidFigure) {
		val fs: FigureState = hf.getFigureState
		trace1("FigureState={}", fs)
		val rotMap: JMap[String, JList[BoneProjectionPosition]] = ModelRobotUtils.getGoalAnglesAsRotations(br)
		trace1("rotMap={}", rotMap)
		applyAllSillyEulerRotations(fs, rotMap)
	}

	def applyAllSillyEulerRotations(fs: FigureState, rotMap: JMap[String, JList[BoneProjectionPosition]]) {
		import scala.collection.JavaConversions._

		for (e <- rotMap.entrySet) {
			val boneName: String = e.getKey
			val bs: BoneState = fs.getBoneState(boneName)
			if (bs != null) {
				val rots: JList[BoneProjectionPosition] = e.getValue
				applySillyEulerRotations(bs, rots)
			}
			else {
				warn1("Can't find boneState for {}", boneName)

			}
		}
	}

	// This is not yet a viable technique, as rotations are not commutative!
	// Also, JME3 has some confusing direction labeling things going on - appears
	// that PITCH, ROLL, YAW are not defined in the traditional manner rel. to X, Y, Z.
	// Needs review!
	private def applySillyEulerRotations(bs: BoneState, rots: JList[BoneProjectionPosition]) {
		import scala.collection.JavaConversions._
		for (rot <- rots) {
			val rotAxis: BoneRotationAxis = rot.getRotationAxis
			val rads: Float = rot.getAngleRadians.asInstanceOf[Float]
			rotAxis match {
				case BoneRotationAxis.X_ROT =>
					bs.rot_X_A3rd = rads
				case BoneRotationAxis.Y_ROT =>
					bs.rot_Y_A1st = rads
				case BoneRotationAxis.Z_ROT =>
					bs.rot_Z_A2nd = rads
			}
		}
	}
}
class VWRoboBoneStatePump extends VarargsLogging {

}
