package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.material.Material

import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.respire.Srtw
import org.friendularity.vwmsg.{VWSCR_CellGrid, VWShapeMaker, VWShapeCreateRq, VWStageRqMsg}

/**
  * Created by Stub22 on 6/22/2016.
  */
trait ExtraStuff extends VarargsLogging {
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

trait VWShaperLogic extends ExtraStuff {
	val shapeMaker = new VWShapeMaker{}

}

class VWShaperActor(myRRC: RenderRegistryClient) extends Actor with VWShaperLogic {
	val rootDeepNode = myRRC.getJme3RootDeepNode(null)
	val rootFlatNode = myRRC.getJme3RootOverlayNode(null)
	val assetMgr = myRRC.getJme3AssetManager(null)

	def receive = {
		case cellGridRq : VWSCR_CellGrid => {
			makeBigGridAndAttach_onRendThrd(myRRC, rootDeepNode)
		}
		case vwsrq: VWShapeCreateRq => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}
