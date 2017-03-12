package org.friendularity.vw.cli.goshcl

import com.jme3.math.ColorRGBA
import org.appdapter.core.name.Ident
import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.infra.util.IdentHlp
import org.friendularity.vw.impl.ta.TARqExtractorHelp
import org.friendularity.vw.mprt.manip.{ManipStatusMsg, ManipDesc, MaybeTransform3D}
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{VWSCR_KnownShape, KnownShapeCreateRqImpl, CompositeMeshyShapeCreateRq, VWClearAllShapes, VWShapeDetachRq, VWShapeAttachRq, ShapeManipRqImpl, VWShapeManipRq, VWSCR_TextBox, VWSCR_Node, VWSCR_ExistingNode, VWShapeCreateRq, VWSCR_MeshyComposite, VWMatDesc, VWMeshDesc, KnowsShapeIDsPart}

/**
  * Created by Owner on 1/23/2017.
  */
trait GeneralXlatorSupport extends IdentHlp with TARqExtractorHelp  {

	lazy val EMPTY_XFORM = new MaybeTransform3D {}

	protected def makeMeshShapeCreateRq(idsPart : KnowsShapeIDsPart, initXform3D : MaybeTransform3D,
							   meshDesc : VWMeshDesc, matDesc : VWMatDesc) : CompositeMeshyShapeCreateRq = {
		val request = new VWSCR_MeshyComposite(idsPart, initXform3D, meshDesc, matDesc)
		request
	}
	protected def makeMeshShapeCreateReq(parentID_opt : Option[Ident], initXform3D : MaybeTransform3D, meshDesc : VWMeshDesc,
										 matDesc : VWMatDesc) : CompositeMeshyShapeCreateRq = {
		val meshShapeID : Ident = makeStampyRandyIdentAnon()
		val shapeKnownIDs = new VWSCR_KnownShape(Some(meshShapeID), parentID_opt)
		makeMeshShapeCreateRq(shapeKnownIDs, initXform3D, meshDesc, matDesc)
	}


}
import com.jme3.scene.{Node => JmeNode}

trait ShaperMsgMaker {

	private def makeRq_createFromJmeNode(exstNode : JmeNode,  nodeID : Ident, knParID_opt : Option[Ident]) :
	VWShapeCreateRq = VWSCR_ExistingNode(exstNode, nodeID, knParID_opt)

	def makeRq_createEmptyJmeNode(nodeID : Ident, knParID_opt : Option[Ident]) :
	VWShapeCreateRq = VWSCR_Node(nodeID, knParID_opt)

	def makeRq_TextBox(shapeID : Ident, contentTxt : String, flatSpc : Boolean, xform  : MaybeTransform3D, txtColor : ColorRGBA) =
				VWSCR_TextBox(Option(shapeID), contentTxt, flatSpc, xform, txtColor)

	def makeRq_Manip(tgtShapeID : Ident, manipDesc : ManipDesc,
					 statusTlr_opt : Option[CPStrongTeller[ManipStatusMsg]]) : VWShapeManipRq =
				ShapeManipRqImpl(tgtShapeID, manipDesc, statusTlr_opt)

	def makeRq_Attach(knownID : Ident, knownParentID_opt : Option[Ident])  = VWShapeAttachRq(knownID, knownParentID_opt)

	def makeRq_Detach(shapeID : Ident) = VWShapeDetachRq(shapeID)

	def makeRq_ClearAll = VWClearAllShapes
}