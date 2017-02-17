package org.friendularity.netcli.vwta

import org.appdapter.core.name.{FreeIdent, Ident}
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.BasicTypedValueMap
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.goody.GoodyNames
import org.friendularity.qpc.{OffersQpidSomething, QPidDestMgrJFlux, JmsDestMgr}
import org.friendularity.vw.mprt.manip.MaybeTransform3D

/**
  * Created by Stub22 on 9/5/2016.
  */
trait OffersVWorldClient extends OffersQpidSomething  with VWTAMsgMaker {
	protected lazy val myClient : QPidTATestClient = {
		val clientDestMgr : JmsDestMgr = new QPidDestMgrJFlux(myQpidConnMgr)
		val client = new QPidTATestClient(clientDestMgr)
		client
	}

	def checkClient() : Unit = {
		info1("Beginning checkClient for offer={}", this)
		val destMgr = myClient.getJmsDestMgr
		info3("Finished checkClient for offer={}, client={}, destMgr={}", this, myClient, destMgr)
	}
	val myPreferredEncoding : Int = myClient.ENCODE_PREF_TRT
	var myMsgSendCnt = 0;
	def sendTARq(taSpec : ThingActionSpec) : Unit = {
		debug1("Sending ta-rq={}", taSpec)
		myClient.sendVWRqThingAct(taSpec, myPreferredEncoding)
		myMsgSendCnt += 1
	}

	def sendEntitySmooveRq(entityID : Ident, typeID: Ident, maybeXform3D : MaybeTransform3D, durSec : Float) : Unit = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		writeXform3D(paramWriter, maybeXform3D)
		paramWriter.putDuration(durSec)
		val taSpec = makeTASpec(entityID, typeID, GoodyNames.ACTION_MOVE, btvm)
		sendTARq(taSpec)
	}
	def sendEntityAbruptMoveRq(entityID : Ident, typeID: Ident, maybeXform3D : MaybeTransform3D) : Unit = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		writeXform3D(paramWriter, maybeXform3D)
		// ACTION_MOVE with no duration, and ACTION_SET are both (equivalently) defined to be "abrupt" moves.
		val taSpec = makeTASpec(entityID, typeID, GoodyNames.ACTION_SET, btvm)
		sendTARq(taSpec)
	}

	val sinbadBodyURI = "urn:ftd:cogchar.org:2012:runtime#char_sinbad_88"
	val sinbadBodyID = new FreeIdent(sinbadBodyURI)
	def sendSinbadSmooveRq(maybeXform3D : MaybeTransform3D, durSec : Float) : Unit = {
		sendEntitySmooveRq(sinbadBodyID, GoodyNames.TYPE_AVATAR, maybeXform3D, durSec)
	}
	def sendSinbadAbruptMoveRq(maybeXform3D : MaybeTransform3D) : Unit = {
		sendEntityAbruptMoveRq(sinbadBodyID, GoodyNames.TYPE_AVATAR, maybeXform3D)
	}

	def sendRq_makeExtraCamera(camGuideShapeID : Ident) : Unit = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		//		writeXform3D(paramWriter, maybeXform3D)
		//		paramWriter.putDuration(durSec)
		val taSpec = makeTASpec(camGuideShapeID, GoodyNames.TYPE_CAMERA, GoodyNames.ACTION_CREATE, btvm)
		info1("Sending Camera make rq={}", taSpec)
		myClient.sendVWRqThingAct(taSpec, myClient.ENCODE_PREF_TRT)
	}
	def sendRq_smooveCameraGuideShape(camGuideShapeID : Ident, xform : MaybeTransform3D, durSec : Float) : Unit = {
		sendEntitySmooveRq(camGuideShapeID, GoodyNames.TYPE_CAMERA, xform, durSec)
	}
	def sendRq_abruptMoveRootCamera(rootCamID : Ident, xform : MaybeTransform3D) : Unit = {
		sendEntityAbruptMoveRq(rootCamID, GoodyNames.TYPE_CAMERA, xform)
	}
	def sendRq_smooveRootCamera(rootCamID : Ident, xform : MaybeTransform3D, durSec : Float) : Unit = {
		sendEntitySmooveRq(rootCamID, GoodyNames.TYPE_CAMERA, xform, durSec)
	}

	def UNUSED_sendRq_bindKnownCamera_UNUSED(knownCamID : Ident, camGuideShapeID : Ident) : Unit = {
		val dfltCamRefID : Ident = GoodyNames.makeID("DFLT_CAM")
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putNameAtName(GoodyNames.SUBCOMPONENT, dfltCamRefID)
		val taSpec = makeTASpec(camGuideShapeID, GoodyNames.TYPE_CAMERA, GoodyNames.ACTION_SET, btvm)
	}
}
class OffersVWorldClientWrapper() extends OffersVWorldClient {
	override val myPreferredEncoding: Int = myClient.ENCODE_PREF_TRT
}