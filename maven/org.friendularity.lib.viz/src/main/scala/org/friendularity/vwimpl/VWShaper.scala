package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.material.Material
import com.jme3.scene.shape.Sphere

import com.jme3.scene.{Node => JmeNode, Geometry, Mesh, Spatial}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.respire.Srtw
import org.friendularity.vwmsg.{VWCleaAllShapes, VWSCR_Sphere, VWSCR_TextBox, VWShapeCreateRq, VWSCR_CellGrid, VWStageRqMsg}

import scala.collection.mutable

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
trait VWSpatialsForShapes extends PatternGridMaker{
	protected def getTooMuchRRC : RenderRegistryClient
	//	val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
	def makeForRq(vwscr : VWShapeCreateRq) : Spatial = {
		vwscr match {
			case txtBox : VWSCR_TextBox => {
				null
			}
			case bigGrid : VWSCR_CellGrid => {
				makeBigGridNode(getTooMuchRRC)
			}
			case meshBased : VWShapeCreateRq => {
				val mesh: Mesh = meshBased match {
					case sph: VWSCR_Sphere => {
						// zSamp, rSamp, radius
						new Sphere(16, 16, sph.myRadius)
					}
				}
				val geomNameArb: String = "geom_from_msg_shape_" + System.currentTimeMillis()
				val geom = new Geometry(geomNameArb, mesh)
				geom
			}
		}
	}
}

case class MadeSpatRec(myID : Ident, mySpat : Spatial, myCreateRq : VWShapeCreateRq)

trait VWShaperLogic extends PatternGridMaker {
	protected def getRRC : RenderRegistryClient
	protected val myTopDeepNode = getRRC.getJme3RootDeepNode(null)
	protected val myTopFlatNode = getRRC.getJme3RootOverlayNode(null)
	protected val myAssetMgr = getRRC.getJme3AssetManager(null)

	val myShapeMaker = new VWSpatialsForShapes{
		override  protected def getTooMuchRRC : RenderRegistryClient = getRRC
	}

	lazy protected val myMadeSpatRecsByID = new mutable.HashMap[Ident, MadeSpatRec]()

	protected def clearAllShapes_onRendThrd(): Unit = {
		for (madeRec <- myMadeSpatRecsByID.values) {
			val madeSpat = madeRec.mySpat
			val parentNode = madeSpat.getParent()
			if (parentNode != null) {
				parentNode.detachChild(madeSpat)
			}
		}
		myMadeSpatRecsByID.clear()
	}
	def makeAndPlace(toMake : VWShapeCreateRq): Unit = {
		val madeSpat = myShapeMaker.makeForRq(toMake)
		val kuri_opt = toMake.getKnownURI_opt
		if (kuri_opt.isDefined) {
			val knownID = new FreeIdent(kuri_opt.get)
			val madeSpatRec = new MadeSpatRec(knownID, madeSpat, toMake)
			val previousMadeSpatRec_opt = myMadeSpatRecsByID.get(knownID)
			if (toMake.expectUniqueURI && previousMadeSpatRec_opt.isDefined) {
				warn2("Found existing shape-spat at oldShape={}, so IGNORING newShape={}",
							previousMadeSpatRec_opt, toMake)
			} else {
				myMadeSpatRecsByID.put(knownID, madeSpatRec)
			}
		}
		val knownParentID_opt = toMake.getKnownParentID_opt
		val knownParentNode_opt : Option[JmeNode] = if (knownParentID_opt.isDefined) {
			val kparid = knownParentID_opt.get
			val knownParentMadeRec_opt = myMadeSpatRecsByID.get(kparid)
			if (knownParentMadeRec_opt.isDefined) {
				val pNode : JmeNode = knownParentMadeRec_opt.get.mySpat.asInstanceOf[JmeNode]
				Option(pNode)
			} else {
				None
			}
		} else None

		val parentToUse_opt = knownParentNode_opt.orElse(if (toMake.inFlatSpace) {
				Option(myTopFlatNode)
			} else {
			Option(myTopDeepNode)
		})
		if (parentToUse_opt.isDefined) {
			parentToUse_opt.get.attachChild(madeSpat)
		}
	}
	def attachBigGrid(): Unit = {
		makeBigGridAndAttach_onRendThrd(getRRC, myTopDeepNode)
	}
}

class VWShaperActor(myRRC: RenderRegistryClient) extends Actor with VWShaperLogic {
	override protected def getRRC : RenderRegistryClient = myRRC

	def receive = {
		case clearAll : VWCleaAllShapes => {
			clearAllShapes_onRendThrd()
		}
		case cellGridRq : VWSCR_CellGrid => {
			attachBigGrid
		}
		case vwsrq: VWShapeCreateRq => {
			makeAndPlace(vwsrq)
		}
	}
}
