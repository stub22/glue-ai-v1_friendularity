package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.material.Material
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.shape.Sphere

import com.jme3.scene.{Node => JmeNode, Geometry, Mesh, Spatial}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.respire.Srtw
import org.friendularity.vwmsg.{SmooveFromCurrent3D, Transform3D, VWMeshyShapeRq, VWClearAllShapes, VWSCR_Sphere, VWSCR_TextBox, VWShapeCreateRq, VWSCR_CellGrid, VWStageRqMsg}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by Stub22 on 6/22/2016.
  */
trait PatternGridMaker extends VarargsLogging {
	// We are currently calling this *off* rndThrd, and it is working.
	def makeBigGridAndAttach_onRendThrd(rrc: RenderRegistryClient, parentNode : JmeNode) : Unit = {
		val ssNode = makeBigGridNode(rrc)
		info2("Made cellGrid node, attaching: {} to {} - which is supposed to be on rendThrd, but conditionally may " +
					"work otherwise, too.", ssNode, parentNode)
		parentNode.attachChild(ssNode) // Question:  Is this the only step that requires us to be onRendThrd?
	}
	def makeBigGridNode(rrc: RenderRegistryClient) : JmeNode = {
		val assetMgr = rrc.getJme3AssetManager(null);
		val someMat = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
		val matPal = new MatPallete(someMat)
		val outerGuy = new OuterGuy(rrc, matPal)
		val srtwInst = new Srtw {
			override def getRRC = rrc
			override def getOuterGuy : OuterGuy = outerGuy

		}
		val uniformCount : Int = 40
		val uniformLen : Float = 800.0f

		// Decide the cross-sections of cells we want to do something with.
		// It is actually OK if these indices are outside the implied range of the deepSpace

		val chosenIdxs_X: Range = 10 to 30 by 2
		val chosenIdxs_Y: Range = 10 to 42 by 4
		val chosenIdxs_Z: Range = -5 to 30 by 5

		val ssNode = srtwInst.makeSheetspace(uniformCount, uniformLen, chosenIdxs_X, chosenIdxs_Y, chosenIdxs_Z)
		ssNode

	}
}
trait SpatMatHelper {
	protected def getTooMuchRRC : RenderRegistryClient
	val rrc = getTooMuchRRC
	val assetMgr = rrc.getJme3AssetManager(null);
	val someMat = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
	val matPal = new MatPallete(someMat)
	val outerGuy = new OuterGuy(rrc, matPal)
	def getBrushJar : BrushJar = outerGuy.myBrushJar


}
trait VWSpatialsForShapes extends PatternGridMaker with SpatMatHelper {


	//	val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
	def makeForRq(vwscr : VWShapeCreateRq) : Spatial = {
		vwscr match {
			case txtBox : VWSCR_TextBox => {
				null
			}
			case bigGrid : VWSCR_CellGrid => {
				makeBigGridNode(getTooMuchRRC)
			}
			case meshBasedRq : VWMeshyShapeRq => {
				val mesh: Mesh = meshBasedRq match {
					case sph: VWSCR_Sphere => {
						// zSamp, rSamp, radius
						new Sphere(16, 16, sph.myRadius)
					}

				}
				val geomNameArb: String = "geom_from_msg_shape_" + System.currentTimeMillis()
				val geom = new Geometry(geomNameArb, mesh)
				applyMat(geom, meshBasedRq)
				applySpatialTransform(geom, meshBasedRq.getCoreParams3D.get)
				geom
			}
		}
	}
	def applyMat(geom : Geometry, mshShpRq : VWMeshyShapeRq) : Unit = {
		val dsc_opt : Option[ColorRGBA] = mshShpRq.getColorParam
		val dsc = dsc_opt.getOrElse(ColorRGBA.Gray)
		val brush = getBrushJar.makeBrush(dsc)
		brush.stroke(geom)
	}
	def applySpatialTransform(spat : Spatial, params : Transform3D) : Unit = {
		val pos : Vector3f = params.getPos
		spat.setLocalTranslation(pos)
		val rot : Quaternion = params.getRotQuat
		spat.setLocalRotation(rot)
		val scl : Vector3f = params.getScale
		spat.setLocalScale(scl)
	}
}


case class MadeSpatRec(mySpat : Spatial, myID_opt : Option[Ident], myCreateRq : VWShapeCreateRq)
			extends Smoovable {
	override def getMainSpat : Spatial = mySpat
	override def getID : Ident = myID_opt.get // Will throw if myID_opt is None!
}

trait IdentHlp {
	val uniqueR = new Random()

	val noSuffix = ""
	val idSuffix = "#id"

	def makeStampyRandyString (prefix : String, suffix : String) : String = {
		val rnum = uniqueR.nextInt(1000 * 1000)
		val tstamp = System.currentTimeMillis()
		String.format("%s%d_%06d%s", prefix, tstamp : java.lang.Long, rnum : Integer, suffix)
	}
	def makeStampyRandyIdent() : Ident = {
		val uriTxt = makeStampyRandyString("urn:sri_", idSuffix)
		val id = new FreeIdent(uriTxt)
		id
	}
}

trait VWShaperLogic extends PatternGridMaker with EnqHlp with IdentHlp {

	protected def getRRC : RenderRegistryClient
	protected val myTopDeepNode : JmeNode = {
		val nodeName = makeStampyRandyString("deep_shape_parent_", noSuffix)
		val tdn = new JmeNode(nodeName)
		val rootN = getRRC.getJme3RootDeepNode(null)
		rootN.attachChild(tdn)
		tdn
	}
	protected val myTopFlatNode : JmeNode = {
		val nodeName = makeStampyRandyString("flat_shape_parent_", noSuffix)
		val tfn = new JmeNode(nodeName)
		val rootN = getRRC.getJme3RootOverlayNode(null)
		rootN.attachChild(tfn)
		tfn
	}

	protected val myAssetMgr = getRRC.getJme3AssetManager(null)

	val myShapeMaker = new VWSpatialsForShapes{
		override  protected def getTooMuchRRC : RenderRegistryClient = getRRC
	}

	lazy protected val myMadeSpatRecsByID = new mutable.HashMap[Ident, MadeSpatRec]()

	protected def clearAllShapes_onRendThrd(): Unit = {
		for (madeRec <- myMadeSpatRecsByID.values) {
			val madeSpat = madeRec.mySpat
			val removed = madeSpat.removeFromParent()
//			val parentNode = madeSpat.getParent()
//			if (parentNode != null) {
//				parentNode.detachChild(madeSpat)
//			}
		}
		myMadeSpatRecsByID.clear()
		myTopDeepNode.detachAllChildren()
		myTopFlatNode.detachAllChildren()
	}
	val makeIdentIfMissing : Boolean = true



	def makeAndPlace(toMake : VWShapeCreateRq): MadeSpatRec = {
		val madeSpat : Spatial = myShapeMaker.makeForRq(toMake)
		val madeSpatRec = registerSpat(madeSpat, toMake)
		val deferredAttachFunc : Function0[Unit] = () => {attachToParent_onRendThrd(madeSpat, toMake)}
		enqueueJmeCallable(getRRC, deferredAttachFunc)
		madeSpatRec
	}
	protected def findMadeSpat(shapeID : Ident) : Option[MadeSpatRec] = {
		myMadeSpatRecsByID.get(shapeID)
	}
	protected def getOrMakeID_opt (toMake : VWShapeCreateRq) : Option[Ident] = {
		val clientSentID_opt = toMake.getKnownID_opt
		clientSentID_opt.orElse(if (makeIdentIfMissing) Some(makeStampyRandyIdent()) else None)
	}
	protected def registerSpat(madeSpat : Spatial, toMake : VWShapeCreateRq): MadeSpatRec = {
		val assignedID_opt = getOrMakeID_opt(toMake)
		val madeSpatRec = new MadeSpatRec(madeSpat, assignedID_opt, toMake)
		// We only mess with the map, if there is no assignedID
		if (assignedID_opt.isDefined) {
			val assignedID: Ident = assignedID_opt.get
			if (toMake.expectEmptySlot) {
				val previousMadeSpatRec_opt = myMadeSpatRecsByID.get(assignedID)

				if (previousMadeSpatRec_opt.isDefined) {
					warn2("Found existing shape-spat at oldShape={}, so IGNORING newShape={}",
						previousMadeSpatRec_opt, toMake)
				}
			}
			info1("Storing madeSpatRec at id={}", assignedID)
			myMadeSpatRecsByID.put(assignedID, madeSpatRec)

		}
		madeSpatRec
	}
	def attachToParent_onRendThrd(madeSpat : Spatial, toMake : VWShapeCreateRq) : Unit = {
		val knownParentID_opt = toMake.getKnownParentID_opt
		val knownParentNode_opt : Option[JmeNode] = if (knownParentID_opt.isDefined) {
			val kparid = knownParentID_opt.get
			val knownParentMadeRec_opt = myMadeSpatRecsByID.get(kparid)
			if (knownParentMadeRec_opt.isDefined) {
				val pNode : JmeNode = knownParentMadeRec_opt.get.mySpat.asInstanceOf[JmeNode]
				Option(pNode)
			} else {
				warn2("Could not find expected parentNode at ID={}, for child={}", kparid, toMake)
				// TODO: Probably we should force failure in this case, not attach to default
				None
			}
		} else None

		val parentToUse_opt : Option[JmeNode] = knownParentNode_opt.orElse(
			if (toMake.inFlatSpace) {
				Option(myTopFlatNode)
			} else {
				Option(myTopDeepNode)
			})

		info2("Using parentNode={} for childSpatial={}", parentToUse_opt, madeSpat)
		val dooda = parentToUse_opt.map(_.attachChild(madeSpat))

	}
	def attachBigGrid_onRendThrd(): Unit = {
		makeBigGridAndAttach_onRendThrd(getRRC, myTopDeepNode)
	}


}

class VWShaperActor(myRRC: RenderRegistryClient) extends Actor with VWShaperLogic {
	override protected def getRRC : RenderRegistryClient = myRRC

	def receive = {
		case clearAll : VWClearAllShapes => {
			val func : Function0[Unit] = () => {clearAllShapes_onRendThrd}
			enqueueJmeCallable(myRRC, func)
		}
		case cellGridRq : VWSCR_CellGrid => {
			val func : Function0[Unit] = () => {attachBigGrid_onRendThrd}
			enqueueJmeCallable(myRRC, func)
		}
		case vwsrq: VWShapeCreateRq => {
			val unusedResult : MadeSpatRec = makeAndPlace(vwsrq)
		}
		case smvFromCur : SmooveFromCurrent3D => {
			val shapeID : Ident = smvFromCur.getTgtShapeID
			val madeSpatRec_opt = findMadeSpat(shapeID)
			madeSpatRec_opt.get.applySmooveFromCurrent_mystThrd(smvFromCur)
		}
	}
}
