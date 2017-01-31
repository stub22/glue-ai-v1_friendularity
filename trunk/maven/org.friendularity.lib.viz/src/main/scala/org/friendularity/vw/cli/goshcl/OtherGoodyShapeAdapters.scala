package org.friendularity.vw.cli.goshcl

import com.jme3.math.ColorRGBA
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.PartialTransform3D
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{VWMatDesc, SimpleMatDesc, VWMD_Box, VWMD_Cylinder, VWMeshDesc}

/**
  * Created by Owner on 1/22/2017.
  */
trait OtherGoodyShapeXlator extends GoodyRqPartialXlator {
//	private val DEFAULT_FLOOR_COLOR: ColorRGBA = ColorRGBA.LightGray

	override def makeCreateRqs(taSpec : ThingActionSpec) : List[VWContentRq]  = {
//	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident,  gax : GoodyActionExtractor) // paramTVM : TypedValueMap)
//				: List[VWContentRq] = {
		val tgtTypeID : Ident = taSpec.getTargetThingTypeID

		val gparentID_opt : Option[Ident] = None

		val parentNodeShapeID = makeStampyRandyIdent("ogParentNode")

		val parentRqs : List[VWContentRq] = makeParentCreateRqs_withXform(parentNodeShapeID, gparentID_opt, taSpec)

		val suppliedMat = translateSimpleMatDesc(taSpec)

		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_BOX => {
				makeRqs_box(Some(parentNodeShapeID), suppliedMat)
			}
			case GoodyNames.TYPE_FLOOR => {
				makeRqs_floor(Some(parentNodeShapeID), suppliedMat)
			}
		}
		parentRqs ::: msgList
	}

	private val brnMatDesc = new SimpleMatDesc(Some(ColorRGBA.Brown))
	private val ylwMatDesc = new SimpleMatDesc(Some(ColorRGBA.Yellow))
	private val ltGryMatDesc = new SimpleMatDesc(Some(ColorRGBA.LightGray))


	def makeRqs_box(parentID_opt : Option[Ident], boxMD : VWMatDesc) : List[VWContentRq] = {
		val boxMeshDesc : VWMeshDesc = new VWMD_Box(1f, 1f, 1f)
		val boxXform = EMPTY_XFORM
		val boxRq = makeMeshShapeCreateReq(parentID_opt, boxXform, boxMeshDesc, boxMD)
		List(boxRq)
	}
	def makeRqs_floor(parentID_opt : Option[Ident], floorMD : VWMatDesc) : List[VWContentRq] = {
		val floorMeshDesc : VWMeshDesc = new VWMD_Box(140f, 0.25f, 140f)
		val floorXform = EMPTY_XFORM
		val floorRq = makeMeshShapeCreateReq(parentID_opt, floorXform, floorMeshDesc, floorMD)
		List(floorRq)

		/*
				  val floorBox : Mesh = new Box(140f, 0.25f, 140f)
				  val plane : Plane = new Plane
				plane.setOriginNormal(new Vector3f(0, 0.25f, 0), Vector3f.UNIT_Y)
				if (color == null) {
		color = DEFAULT_COLOR
		}
				addGeometry(floorBox, null, color, new Quaternion, new PlaneCollisionShape(plane), 0f)

		 */
	}

}
