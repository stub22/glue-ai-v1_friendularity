package org.friendularity.vw.cli.goshcl

import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.Mesh
import com.jme3.scene.shape.Cylinder
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.TypedValueMap
import org.cogchar.name.goody.GoodyNames
import org.friendularity.util.IdentHlp
import org.friendularity.vw.mprt.manip.PartialTransform3D
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{KnownShapeCreateRqImpl, VWSCR_MeshyCmpnd, SimpleMatDesc, VWSCR_Cylinder}

/**
  * Created by Owner on 1/22/2017.
  */
trait TicTacShapeXlator extends GoodyRqPartialXlator {
	lazy val myGridAdapter = new TTGridAdapter {}
	lazy val myMarkAdapter = new TTMarkAdapter {}
	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = {
		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_TICTAC_GRID => {
				myGridAdapter.makeFourIntersectingPostCylRqs
			}
			case GoodyNames.TYPE_TICTAC_MARK => {
				val flagIsO = false

				Nil
			}
		}
		msgList
	}
}
trait TTGridAdapter extends IdentHlp {
	private val DEFAULT_GRID_COLOR: ColorRGBA = ColorRGBA.Blue
	private val SIZE_MULTIPLIER: Float = 9f
	private val ROTATE_UPRIGHT: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)

	def makeFourIntersectingPostCylRqs : List[VWContentRq] = {
		//
		// val gridLeg : Mesh = new Cylinder(20, 20, 1f / 5f, SIZE_MULTIPLIER, true)

		val offsetDistance: Float = SIZE_MULTIPLIER / 6f

		val axisSampCnt: Int = 20
		val radialSampCnt: Int = 20
		val radiusF: Float = 1f
		val heightF: Float = 1f / 5f
		val flgClosed = true
		val cylMeshDesc = new VWSCR_Cylinder(axisSampCnt, radialSampCnt, radiusF, heightF, flgClosed)

		val aqua : ColorRGBA = new ColorRGBA(0.1f,1.0f,0.5f, 0.65f)
		val cylCol : ColorRGBA = aqua
		val cylMatDesc = new SimpleMatDesc(Some(cylCol))

		val cylPos = new Vector3f(-15.0f, 12.0f, 4.0f) // biggish dirigible
		val cylRot = Quaternion.IDENTITY
		val cylXform = new PartialTransform3D(Some(cylPos), Some(cylRot), Some(Vector3f.UNIT_XYZ))

		val knownSphereID_opt : Option[Ident] = Some(makeStampyRandyIdentAnon())
		val parentID_opt = None
		val msgPart_known = new KnownShapeCreateRqImpl(knownSphereID_opt, parentID_opt)

		val cylRq1 = new VWSCR_MeshyCmpnd(msgPart_known, cylXform, cylMeshDesc, cylMatDesc)

		List(cylRq1)
	}
}
trait TTMarkAdapter {
	val xRotationAngles: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)
	def makeRqs_TorusForO : List[VWContentRq] = {
		Nil
	}
	def makeRqs_CrossedCylsForX : List[VWContentRq] = {
		Nil
	}

}

