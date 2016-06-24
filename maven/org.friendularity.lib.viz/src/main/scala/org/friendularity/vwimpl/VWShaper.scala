package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.material.Material
import com.jme3.scene.shape.Sphere

import com.jme3.scene.{Node => JmeNode, Geometry, Mesh, Spatial}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.respire.Srtw
import org.friendularity.vwmsg.{VWSCR_Sphere, VWSCR_TextBox, VWShapeCreateRq, VWSCR_CellGrid, VWStageRqMsg}

/**
  * Created by Stub22 on 6/22/2016.
  */
trait PatternGridMaker extends VarargsLogging {
	// We are currently calling this *off* rndThrd, and it is working.
	def makeBigGridAndAttach_onRendThrd(rrc: RenderRegistryClient, parentNode : JmeNode) : Unit = {
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
		info2("Made cellGrid node, attaching: {} to {} - which is supposed to be on rendThrd, but conditionally may " +
					"work otherwise, too.", ssNode, parentNode)
		parentNode.attachChild(ssNode) // Question:  Is this the only step that requires us to be onRendThrd?
	}
}
trait VWSpatialsForShapes {
	//	val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
	def makeForRq(vwscr : VWShapeCreateRq) : Spatial = {
		vwscr match {
			case txtBox : VWSCR_TextBox => {
				null
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

trait VWShaperLogic extends PatternGridMaker {
	protected def getRRC : RenderRegistryClient
	protected val myTopDeepNode = getRRC.getJme3RootDeepNode(null)
	protected val myTopFlatNode = getRRC.getJme3RootOverlayNode(null)
	protected val myAssetMgr = getRRC.getJme3AssetManager(null)

	val myShapeMaker = new VWSpatialsForShapes{}

	def makeAndPlace(toMake : VWShapeCreateRq): Unit = {
		val madeSpat = myShapeMaker.makeForRq(toMake)

		if (toMake.inFlatSpace) {
			myTopFlatNode.attachChild(madeSpat)
		} else {
			myTopDeepNode.attachChild(madeSpat)
		}
	}
	def attachBigGrid(): Unit = {
		makeBigGridAndAttach_onRendThrd(getRRC, myTopDeepNode)
	}
}

class VWShaperActor(myRRC: RenderRegistryClient) extends Actor with VWShaperLogic {
	override protected def getRRC : RenderRegistryClient = myRRC

	def receive = {
		case cellGridRq : VWSCR_CellGrid => {
			attachBigGrid
		}
		case vwsrq: VWShapeCreateRq => {
			makeAndPlace(vwsrq)
		}
	}
}
