package org.friendularity.vw.cli.goshcl

import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.Mesh
import com.jme3.scene.shape.Cylinder
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.util.IdentHlp
import org.friendularity.vw.mprt.manip.{ManipDesc, AbruptManipAbsPartialImpl, MaybeTransform3D, PartialTransform3D}
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{ShapeManipRqImpl, VWMD_Torus, VWSCR_Node, CompositeMeshyShapeCreateRq, VWMatDesc, VWMeshDesc, KnownShapeCreateRqImpl, VWSCR_MeshyComposite, SimpleMatDesc, VWMD_Cylinder}

/**
  * Created by Owner on 1/22/2017.
  */
trait TicTacShapeXlator extends GoodyRqPartialXlator {
	lazy val myGridAdapter = new TTGridAdapter {}
	lazy val myMarkAdapter = new TTMarkAdapter {}

	private val ROTATE_UPRIGHT_EULER : Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)
	private val rotUpQuat = new Quaternion(ROTATE_UPRIGHT_EULER)

	override def makeCreateRqs(taSpec : ThingActionSpec) : List[VWContentRq]  = {
//	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident,  gax : GoodyActionExtractor) // paramTVM : TypedValueMap)
//				: List[VWContentRq] = {
		val tgtTypeID : Ident = taSpec.getTargetThingTypeID
		val parentNodeShapeID = makeStampyRandyIdent("ttParentNode")

		val parentRqs = makeParentCreateRqs(parentNodeShapeID, taSpec)

		val matDesc = translateSimpleMatDesc(taSpec)

		val gax = new GoodyActionExtractor(taSpec)

		val childRqs : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_TICTAC_GRID => {
				val cylCrtRqs = myGridAdapter.makePostCylShpRqs(Some(parentNodeShapeID), matDesc)
				cylCrtRqs
			}
			case GoodyNames.TYPE_TICTAC_MARK => {

				val flagIsO = gax.getSpecialBoolean(GoodyNames.USE_O)
				if (flagIsO) {
					myMarkAdapter.makeRqs_TorusForO(Some(parentNodeShapeID), matDesc)
				} else {
					myMarkAdapter.makeRqs_X(Some(parentNodeShapeID), matDesc)
				}
			}
		}
		val fullRqList = parentRqs ::: childRqs
		info1("FullRqList for TicTac goody create={}", fullRqList)
		fullRqList
	}
}
trait TTGridAdapter extends GeneralXlatorSupport {
	private val DEFAULT_GRID_COLOR: ColorRGBA = ColorRGBA.Blue
	private val SIZE_MULTIPLIER: Float = 9f
	private val ROTATE_UPRIGHT: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)

/*	def makeTicTacGridShapeRqs : List[VWContentRq] = {
		val parentNodeShapeID = makeStampyRandyIdent("ttGridParent")
		val parentCreateRq = new VWSCR_Node(parentNodeShapeID, None)
		val childCylRqs : List[VWContentRq] = makePostCylShpRqs(parentNodeShapeID)
		List(parentCreateRq : VWContentRq) ::: childCylRqs
	}
	*/
	def makePostCylShpRqs(parentShapeID_opt : Option[Ident], ttMD : VWMatDesc) : List[VWContentRq] = {
		//
		// val gridLeg : Mesh = new Cylinder(20, 20, 1f / 5f, SIZE_MULTIPLIER, true)

		val offsetDistance: Float = SIZE_MULTIPLIER / 6f

		val axisSampCnt: Int = 20
		val radialSampCnt: Int = 20
		val radiusF: Float = 1f / 5f
		val heightF: Float = SIZE_MULTIPLIER
		val flgClosed = true
		val cylMeshDesc = new VWMD_Cylinder(axisSampCnt, radialSampCnt, radiusF, heightF, flgClosed)

//		val aqua : ColorRGBA = new ColorRGBA(0.1f,1.0f,0.5f, 0.65f)
//		val cylCol : ColorRGBA = aqua
		val cylMatDesc = ttMD // new SimpleMatDesc(Some(cylCol))

		val centerVec = Vector3f.ZERO
		val cylPos_01 = centerVec.add(offsetDistance, 0f, 0f)
		val cylPos_02 = centerVec.add(-offsetDistance, 0f, 0f)

		val cylPos_03 = centerVec.add(0f, 0f, offsetDistance)
		val cylPos_04 = centerVec.add(0f, 0f, -offsetDistance)

		val cylRot_A = Quaternion.IDENTITY
		val rotate90DegAroundY: Quaternion = new Quaternion
		rotate90DegAroundY.fromAngleAxis(Math.PI.toFloat / 2, new Vector3f(0f, 1f, 0f))
		val cylRot_B = rotate90DegAroundY

		val cylXform_01 = new PartialTransform3D(Some(cylPos_01), Some(cylRot_A), Some(Vector3f.UNIT_XYZ))
		val cylXform_02 = new PartialTransform3D(Some(cylPos_02), Some(cylRot_A), Some(Vector3f.UNIT_XYZ))
		val cylXform_03 = new PartialTransform3D(Some(cylPos_03), Some(cylRot_B), Some(Vector3f.UNIT_XYZ))
		val cylXform_04 = new PartialTransform3D(Some(cylPos_04), Some(cylRot_B), Some(Vector3f.UNIT_XYZ))

		val cylRq_01 = makeMeshShapeCreateReq(parentShapeID_opt, cylXform_01, cylMeshDesc, cylMatDesc)
		val cylRq_02 = makeMeshShapeCreateReq(parentShapeID_opt, cylXform_02, cylMeshDesc, cylMatDesc)
		val cylRq_03 = makeMeshShapeCreateReq(parentShapeID_opt, cylXform_03, cylMeshDesc, cylMatDesc)
		val cylRq_04 = makeMeshShapeCreateReq(parentShapeID_opt, cylXform_04, cylMeshDesc, cylMatDesc)

		List(cylRq_01, cylRq_02, cylRq_03, cylRq_04)
	}

}
trait TTMarkAdapter extends GeneralXlatorSupport {
	private val X_DFLT_COLOR: ColorRGBA = ColorRGBA.Black
	private val xDfltMatDesc = new SimpleMatDesc(Some(X_DFLT_COLOR))

	private val O_DFLT_COLOR: ColorRGBA = ColorRGBA.Red
	private val oDfltMatDesc = new SimpleMatDesc(Some(O_DFLT_COLOR))

	private var playerO: Boolean = false
	private var indexX: Int = 0
	private var indexO: Int = 0

	def makeRqs_TorusForO(parentID_opt : Option[Ident], markMD : VWMatDesc) : List[VWContentRq] = {
		val oMeshDesc : VWMeshDesc = new VWMD_Torus(40, 20, 1f/5f, 5f/6f)
		val oXform_01 = EMPTY_XFORM
		val oRq_01 = makeMeshShapeCreateReq(parentID_opt, oXform_01, oMeshDesc, markMD) //  oDfltMatDesc)
		List(oRq_01)
	}

	// val xRotationAngles: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)
	def makeRqs_X(gparentID_opt : Option[Ident], markMD : VWMatDesc) : List[VWContentRq] = {
		val parentNodeShapeID = makeStampyRandyIdent("xMarkParent")
		val parentCreateRq = new VWSCR_Node(parentNodeShapeID, gparentID_opt)
		val xLegRqs : List[VWContentRq] = makeRqs_CrossedCylsForX(Some(parentNodeShapeID), markMD)
		// TODO: Set initial XForm (or later manip) for the overall X-rotation.
		List(parentCreateRq : VWContentRq) ::: xLegRqs
	}

	def makeRqs_CrossedCylsForX(parentID_opt : Option[Ident], markMD : VWMatDesc) : List[VWContentRq] = {
		val xLegMeshDesc : VWMeshDesc = new VWMD_Cylinder(20, 20, 1f / 5f, 2.25f, true)

		val rotate45DegAroundY: Quaternion = new Quaternion
		rotate45DegAroundY.fromAngleAxis(Math.PI.toFloat / 4, new Vector3f(0f, 1f, 0f))

		val legXform_01 = new PartialTransform3D(None, Some(rotate45DegAroundY), None)
		val legXform_02 = new PartialTransform3D(None, Some(rotate45DegAroundY.inverse), None)

		val legRq_01 = makeMeshShapeCreateReq(parentID_opt, legXform_01, xLegMeshDesc, markMD)
		val legRq_02 = makeMeshShapeCreateReq(parentID_opt, legXform_02, xLegMeshDesc, markMD)

		List(legRq_01, legRq_02)
	}

}

