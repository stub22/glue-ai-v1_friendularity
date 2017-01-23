package org.friendularity.vw.cli.goshcl

import com.jme3.math.ColorRGBA
import com.jme3.scene.Mesh
import com.jme3.scene.shape.Cylinder
import org.appdapter.core.name.Ident
import org.cogchar.name.goody.GoodyNames
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.VWSCR_Cylinder

/**
  * Created by Owner on 1/22/2017.
  */
trait TicTacShapeXlator extends GoodyRqPartialXlator {
	lazy val myGridAdapter = new TTGridAdapter {}
	lazy val myMarkAdapter = new TTMarkAdapter {}
	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident) : List[VWContentRq] = {
		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_TICTAC_GRID => {
				myGridAdapter.makeFourIntersectingPostCylRqs
			}
			case GoodyNames.TYPE_TICTAC_MARK => {
				Nil
			}
		}
		msgList
	}
}
trait TTGridAdapter {
	private val DEFAULT_GRID_COLOR: ColorRGBA = ColorRGBA.Blue
	private val SIZE_MULTIPLIER: Float = 9f
	private val ROTATE_UPRIGHT: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)

	def makeFourIntersectingPostCylRqs : List[VWContentRq] = {
		//
		val gridLeg : Mesh = new Cylinder(20, 20, 1f / 5f, SIZE_MULTIPLIER, true)

		val offsetDistance: Float = SIZE_MULTIPLIER / 6f

		val axisSampCnt: Int = 20
		val radialSampCnt: Int = 20
		val radiusF: Float = 1f
		val heightF: Float = 1f / 5f
		val flgClosed = true
		val cylRq1 = new VWSCR_Cylinder(axisSampCnt, radialSampCnt, radiusF, heightF, flgClosed)
		List(cylRq1)
	}
}
trait TTMarkAdapter {
	val xRotationAngles: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)
	def makeTorusForO : List[VWContentRq] = {
		Nil
	}
	def makeCrossedCylsForX : List[VWContentRq] = {
		Nil
	}

}

