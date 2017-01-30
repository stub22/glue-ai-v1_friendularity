package org.friendularity.vw.cli.goshcl

import com.jme3.math.{Quaternion, ColorRGBA}
import com.jme3.scene.Mesh
import com.jme3.scene.shape.{Cylinder, Torus}
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.cogchar.render.sys.task.Queuer
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/22/2017.
  */
trait BitGoodyShapeXlator extends GoodyRqPartialXlator {
	private val FALSE_COLOR: ColorRGBA = ColorRGBA.Blue
	private val TRUE_COLOR: ColorRGBA = ColorRGBA.Red
	private var zeroIndex: Int = 0
	private var oneIndex: Int = 0

	def whatvr : Unit = {
		val zeroMesh: Mesh = new Torus(40, 20, 1f / 5f, 5f / 6f)
		val oneMesh: Mesh = new Cylinder(20, 20, 1f / 5f, 2f, true)
		//zeroIndex = addGeometry(zeroMesh, FALSE_COLOR)
		val oneRotationAngles: Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)
		//oneIndex = addGeometry(oneMesh, TRUE_COLOR, new Quaternion(oneRotationAngles))
		//state = boxState
	}

	def setState(boxState: Boolean, qStyle: Queuer.QueueingStyle) {
		val geometryIndex: Int = if (boxState) oneIndex else zeroIndex
		//setGeometryByIndex(geometryIndex, qStyle)
		//state = boxState
	}
	override def makeCreateRqs(taSpec : ThingActionSpec) : List[VWContentRq]  = {
//	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident,  gax : GoodyActionExtractor)//  paramTVM : TypedValueMap)
//				: List[VWContentRq] = {

		val tgtTypeID : Ident = taSpec.getTargetThingTypeID

		val gparentID_opt : Option[Ident] = None
		val parentNodeShapeID = makeStampyRandyIdent("ogParentNode")

		val parentRqs : List[VWContentRq] = makeParentCreateRqs_withXform(parentNodeShapeID, gparentID_opt, taSpec)

		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_BIT_BOX => {
				makeRqs_bitBox(Some(parentNodeShapeID))
			}
			case GoodyNames.TYPE_BIT_CUBE => {
				makeRqs_bitCube(Some(parentNodeShapeID))
			}
		}
		parentRqs ::: msgList
	}

	def makeRqs_bitBox(parentID_opt : Option[Ident]) : List[VWContentRq] = {
		Nil
	}
	def makeRqs_bitCube(parentID_opt : Option[Ident]) : List[VWContentRq] = {
		Nil
	}


}
/*
		  val atlas : TextureAtlas = new TextureAtlas(400, 1200)
		  val matFact : MatFactory = getRenderRegCli.getOpticMaterialFacade(null, null)
		  val assetMgr : AssetManager = matFact.getAssetManager
		  val zeroTexture : Texture = assetMgr.loadTexture("textures/robosteps/Zero.png")
		  val oneTexture : Texture = assetMgr.loadTexture("textures/robosteps/One.png")
		  val blankTexture : Texture = assetMgr.loadTexture("textures/robosteps/BlankGray.png")
		atlas.addTexture(zeroTexture, "ColorMap")
		atlas.addTexture(oneTexture, "ColorMap")
		atlas.addTexture(blankTexture, "ColorMap")

 */