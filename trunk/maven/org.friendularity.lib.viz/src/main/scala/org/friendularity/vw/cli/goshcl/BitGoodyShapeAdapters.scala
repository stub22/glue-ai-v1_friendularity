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
import org.friendularity.vw.msg.shp.deep.{VWMD_TexturedBox, VWMatDesc, VWMD_Box, VWMeshDesc}

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
		val parentNodeShapeID = makeStampyRandyIdent("bitgParentNode")

		val parentRqs : List[VWContentRq] = makeParentCreateRqs_withXform(parentNodeShapeID, gparentID_opt, taSpec)

		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_BIT_BOX => {
				makeRqs_bitBox(Some(parentNodeShapeID))
			}
			case GoodyNames.TYPE_BIT_CUBE => {
				val suppliedMat = translateSimpleMatDesc(taSpec)
				makeRqs_bitCube(Some(parentNodeShapeID), suppliedMat)
			}
		}
		parentRqs ::: msgList
	}

	def makeRqs_bitBox(parentID_opt : Option[Ident]) : List[VWContentRq] = {
		Nil
	}
	def makeRqs_bitCube(parentID_opt : Option[Ident], bcMatDesc : VWMatDesc) : List[VWContentRq] = {

		val boxMeshDesc : VWMeshDesc = new VWMD_TexturedBox(1f, 1f, 1f)
		val boxXform = EMPTY_XFORM
		val boxRq = makeMeshShapeCreateReq(parentID_opt, boxXform, boxMeshDesc, bcMatDesc)
		List(boxRq)

	}


}
/*
		  val cubeMesh : Mesh = new TG_BitCubeBox(1f, 1f, 1f)

 class TG_BitCubeBox extends Box {
	private   var NEW_GEOMETRY_TEXTURE_DATA : Array[Float] = Array(1, 0.333333f, 0, 0.333333f, 0, 0.666667f, 1, 0.666667f, 1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0.333333f, 1, 0.333333f, 1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, 1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, 1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1)

	private static float[] NEW_GEOMETRY_TEXTURE_DATA = {
        1, 0.333333f, 0, 0.333333f, 0, 0.666667f, 1, 0.666667f, // back
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, // right
        1, 0, 0, 0, 0, 0.333333f, 1, 0.333333f, // front
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, // left
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, // top
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1  // bottom
    };

	  def this(x : Float, y : Float, z : Float){
this()
`super`(x, y, z)
}

	protected override   def duUpdateGeometryTextures{
if (getBuffer(Type.TexCoord) == null) {
setBuffer(Type.TexCoord, 2, BufferUtils.createFloatBuffer(NEW_GEOMETRY_TEXTURE_DATA))
}
}
}

		  val cubeMesh : Mesh = new TG_BitCubeBox(1f, 1f, 1f)

		  val atlas : TextureAtlas = new TextureAtlas(400, 1200)

		    val matFact : MatFactory = getRenderRegCli.getOpticMaterialFacade(null, null)
		  val assetMgr : AssetManager = matFact.getAssetManager

		 	  val zeroTexture : Texture = assetMgr.loadTexture("textures/robosteps/Zero.png")
		  val oneTexture : Texture = assetMgr.loadTexture("textures/robosteps/One.png")
		  val blankTexture : Texture = assetMgr.loadTexture("textures/robosteps/BlankGray.png")

			atlas.addTexture(zeroTexture, "ColorMap")
		atlas.addTexture(oneTexture, "ColorMap")
		atlas.addTexture(blankTexture, "ColorMap")

		  val cubeMaterial : Material = matFact.makeMatWithOptTexture("Common/MatDefs/Misc/Unshaded.j3md", "ColorMap", atlas.getAtlasTexture("ColorMap"))

		return addGeometry(cubeMesh, cubeMaterial, null, new Quaternion)



	private   def getStateAdjustedRotationOffset(boxState : Boolean) : Quaternion = {
  val yaw : Float = if (boxState) Math.PI.toFloat else 0f
return new Quaternion().fromAngles(0f, yaw, 0f)
}

	  def setState(boxState : Boolean, qStyle : Queuer.QueueingStyle){
if (boxState != state) {
  val initialRotation : Quaternion = getRotation
  val position : Vector3f = getPosition
  val scale : Vector3f = getScale
moveViaAnimation(position, initialRotation.mult(new Quaternion().fromAngles(0f, Math.PI.toFloat, 0f)), scale, STATE_TRANSITION_TIME)
setNewGeometryRotationOffset(geomIndex, getStateAdjustedRotationOffset(boxState))
setRotation(initialRotation, qStyle)
state = boxState
}
}

 */