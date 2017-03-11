package org.friendularity.vw.cli.goshcl

import akka.actor.{ActorContext, ActorRef}
// import com.jme3.math.ColorRGBA
import com.jme3.scene.{Mesh, Node}
// import com.jme3.scene.shape.Cylinder
// import org.appdapter.core.name.Ident
import org.cogchar.api.thing.ThingActionSpec
// import org.cogchar.name.goody.GoodyNames
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vw.mprt.manip.{ManipStatusMsg, ManipDesc}
import org.friendularity.vw.msg.cor.VWContentRq
// import org.friendularity.vw.msg.shp.deep.{VWMD_Cylinder, VWSCR_TextBox, VWSCR_Node, VWSCR_ExistingNode, ShapeManipRqImpl, VWClearAllShapes, VWShapeDetachRq, VWShapeManipRq, VWShapeClearRq, VWShapeCreateRq, VWShapeAttachRq}

/**
  * Created by Stub22 on 1/19/2017.
  */

trait GoodyShapcliLogic {
	//
	val myGoodyXlator = new GoodyRqToShaperRqTranslator {} // Delegate: Refine and extend as needed from here.

	var myShprTlr_opt : Option[CPStrongTeller[VWContentRq]] = None
	// This state-aware method connects to a known shaper
	def setupOnceWithShaper(shaprTlr : CPStrongTeller[VWContentRq]): Unit = {
		myShprTlr_opt = Some(shaprTlr)
	}
	def processVWGoodyTA_usingShaperMsgs(actSpec : ThingActionSpec, slfActr : ActorRef,
								   localActorCtx : ActorContext): Unit = {

		val trnsltdMsgList  : List[VWContentRq] = myGoodyXlator.makeContentRqsFromTA(actSpec, 0)
		val shprTlr = myShprTlr_opt.get
		trnsltdMsgList.map(shprTlr.tellStrongCPMsg(_))
	}

}
// We have access to 5
// When ignoring msgs, ideal is to print only a small number of "hey, ignoring this input" messages, on ~
// 1st, 11th, 55th, 101st, 505th, 1011th ignored cli msg.
//
