package org.friendularity.vwgoody

import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{TypedValueMap, ThingActionSpec, SerTypedValueMap}
import org.cogchar.api.vworld.{GoodyActionParamReader, GoodyActionParamWriter}
import org.cogchar.impl.thing.basic.{BasicTypedValueMap, BasicThingActionSpec}
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.dir.NamespaceDir
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.{OffersVWorldClient, VWTAMsgMaker}
import java.lang.{Float => JFloat, Integer => JInt, Boolean => JBool}

import org.friendularity.vwimpl.IdentHlp

import scala.util.Random

/**
  * Created by Stub22 on 12/15/2016.
  */
trait Hey extends VWTAMsgMaker with VarargsLogging {

	// entity-ID is a bookeeping handle, of fixed type
	// type and verb help define meaning of params
	// params are leaf data describing an initial or updated (partial) state, except on verb=delete
	val dummyMakerForType = 0
	def makeMakerForSpec: Unit = {
	//	makeTASpec(entityID : Ident, typeID : Ident, verbID : Ident, paramSerMap: SerTypedValueMap) : ThingActionSpec = {

	}
	def makeTTGridSpec(entityID: Ident, verbID: Ident,
					   paramSerMap: SerTypedValueMap): ThingActionSpec = {

		makeTASpec(entityID, GoodyNames.TYPE_TICTAC_GRID, verbID, paramSerMap)
	}
	//val specMakerForType = makeTASpec(_ : Ident, typeID : Ident, _ : Ident, _: SerTypedValueMap)
	val specMakerForTTGrid = makeTASpec(_ : Ident, GoodyNames.TYPE_TICTAC_GRID, _ : Ident, _: SerTypedValueMap)
	val specMakerForTTMark = makeTASpec(_ : Ident, GoodyNames.TYPE_TICTAC_MARK, _ : Ident, _: SerTypedValueMap)
	// val secMakerForBox

	def makeLoc3Params (x : Float, y: Float, z: Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putLocation(x, y, z)
		btvm
	}
	// delta is added to existing param, or
	def adjustFloatParam(tgtParams : TypedValueMap, paramID : Ident, delta : Float) : Unit = {
		val oldVal_orNull : JFloat = tgtParams.getAsFloat(paramID)
		val oldVal : Float = if (oldVal_orNull == null) 0.0f else oldVal_orNull
		var nwVal = oldVal + delta
		tgtParams.putValueAtName(paramID, nwVal)
	}
	def makeDurParam(durSec : Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putDuration(durSec)
		btvm
	}
	def setDurParam(btvm : SerTypedValueMap, durSec : Float) : SerTypedValueMap = {
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putDuration(durSec)
		btvm
	}
	def makeRotParam(magDeg : Float, axisX : Float, axisY : Float, axisZ : Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putRotation(axisX, axisY, axisZ, magDeg)
		btvm
	}
	def makeColorParam(r : Float, g : Float, b: Float, a: Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putColor(r, g, b, a)
		btvm
	}
	def makeBoolStateParam(stateVal : Boolean)  : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putObjectAtName(GoodyNames.BOOLEAN_STATE, stateVal)
		btvm
	}
	def makeXOStateParam(stateIsO : Boolean)  : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putObjectAtName(GoodyNames.USE_O, stateIsO)
		btvm
	}
	def absorbParams(absorber : TypedValueMap, src : TypedValueMap) : Unit = {
		val srcKeysIt = src.iterateKeys()
		while (srcKeysIt.hasNext) {
			val srcKey : Ident = srcKeysIt.next()
			val srcVal : Object = src.getRaw(srcKey)
			absorber.putValueAtName(srcKey, srcVal)
		}
	}
	// sources are absorbed in order, so last source wins.
	def combineParams(srcs : List[TypedValueMap]) : SerTypedValueMap = {
		val result : BasicTypedValueMap  = new ConcreteTVM()
		srcs.foreach(absorbParams(result, _))
		result
	}
	def duplicateParams(srcParams : TypedValueMap) : SerTypedValueMap = {
		combineParams(List(srcParams))
	}
}
class UpdatableGoodyRef(goodyID : Ident, typeID : Ident, initTASpec : ThingActionSpec,
						initParams : SerTypedValueMap) extends VWTAMsgMaker {

	private var		myLastParams : SerTypedValueMap = initParams
	private var		myLastTASpec : ThingActionSpec = initTASpec

	def getLastParams : SerTypedValueMap = myLastParams
	def makeReqAndUpdate(verbID : Ident, nextParams : SerTypedValueMap) : ThingActionSpec = {
		myLastParams = nextParams
		myLastTASpec = makeTASpec(goodyID, typeID, verbID, nextParams)
		myLastTASpec
	}
	def makeDeleteReq : ThingActionSpec = {
		val emptyParams = new ConcreteTVM()
		makeTASpec(goodyID, typeID, GoodyNames.ACTION_DELETE, emptyParams)
	}
}
trait Buncher extends IdentHlp with Hey with VWTAMsgMaker{
	def makeSomeUniqueIdents(idCount : Int, nickPrefix : String) : List[Ident] = {
		var idx = 0

		val indices : Range = 0 to (idCount - 1)
		// TODO:  Format digit string with leading 0s
		val nicks = indices.map(nickPrefix + _)
		val idents = nicks.map(makeStampyRandyIdent(_)).toList
		idents
	}
}
trait TTGridTestMsgMaker extends VWTAMsgMaker {

	def makeTTGridSpec(entityID : Ident, verbID : Ident,
					   paramSerMap: SerTypedValueMap) : ThingActionSpec = {

		makeTASpec(entityID, GoodyNames.TYPE_TICTAC_GRID, verbID, paramSerMap)
	}

	def makeTAS_TTGrid_Create(gridID : Ident, x : Float, y: Float, z: Float): ThingActionSpec = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putLocation(x, y, z)
		makeTTGridSpec(gridID, GoodyNames.ACTION_CREATE, btvm)
	}
	def makeTAS_TTGrid_Move(durSec : Float, gridID : Ident, x : Float, y: Float, z: Float): ThingActionSpec = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putLocation(x, y, z)
		paramWriter.putDuration(durSec)
		makeTTGridSpec(gridID, GoodyNames.ACTION_MOVE, btvm)
	}
	def makeTAS_TTGrid_Delete(gridID : Ident): ThingActionSpec = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		makeTTGridSpec(gridID, GoodyNames.ACTION_DELETE, btvm)
	}

}

class TTGRidBurstTest(ovwc : OffersVWorldClient) extends TTGridTestMsgMaker with VarargsLogging {
	val gridCount = 22
	val delayMsec = 500
	def sendEm(midFix : String) : Unit = {
		val idxes : Range = 0 to (gridCount - 1)

		val yPos = 8.0f
		val zPos = 2.0f
		val prefix = "ttbrst_" + midFix + "_"
		val names : Array[String] = idxes.map(prefix + _).toArray
		val entIDs : Array[Ident] = names.map(n => {new FreeIdent(NamespaceDir.CCRT_NS + n) })
		info1("Made entityIDs={}", entIDs)
		for (idx <- idxes) {
			val xPos = -1.0f * gridCount + 2.0f * idx
			val entID = entIDs(idx)
			val ta = makeTAS_TTGrid_Create(entID, xPos, yPos, zPos)
			info1("Sending TA: {}", ta)
			ovwc.sendTARq(ta)
			Thread.sleep(delayMsec)
		}
		for (idx <- idxes) {
			val xPos = 0.0f
			val yPos = 2.0f * idx
			val entID = entIDs(idx)
			val moveDurSec = 8.0f
			val ta = makeTAS_TTGrid_Move(moveDurSec, entID, xPos, yPos, zPos)
			info1("Sending TA: {}", ta)
			ovwc.sendTARq(ta)
			Thread.sleep(delayMsec)
		}
		for (idx <- idxes) {
			val entID = entIDs(idx)
			val ta = makeTAS_TTGrid_Delete(entID)
			info1("Sending TA: {}", ta)
			ovwc.sendTARq(ta)
			Thread.sleep(delayMsec)
		}
	}
}

class OneBurst(entIdPrfx : String, burstWidth : Int, burstLen : Int) extends Buncher {
	lazy val myIdents : List[Ident] = makeSomeUniqueIdents(burstWidth, entIdPrfx)

	var myRefs : List[UpdatableGoodyRef] = Nil

	def delayAsNeeded(delayMsec_opt : Option[Int]) : Unit = {
		if (delayMsec_opt.isDefined) {
			Thread.sleep(delayMsec_opt.get)
		}
	}
	// TODO:  Wrap up dyna pairs in a class, then pass a collection
	def createGoodies(ovwc : OffersVWorldClient, typeID : Ident, seedParams : SerTypedValueMap,
					  dynaParamID : Ident, dynaDelta : Float, delayMsec_opt : Option[Int]) : Unit = {
		if (myRefs.nonEmpty) {
			throw new Exception("Cant create goodies - refs already exist!")
		}
		var lastParams = seedParams
		myRefs = myIdents.map(goodyID => {
			val nxtParams = duplicateParams(lastParams)
			adjustFloatParam(nxtParams, dynaParamID, dynaDelta)
			val taRq = makeTASpec(goodyID, typeID, GoodyNames.ACTION_CREATE, nxtParams)
			ovwc.sendTARq(taRq)
			lastParams = nxtParams
			delayAsNeeded(delayMsec_opt)
			val ugRef = new UpdatableGoodyRef(goodyID, typeID, taRq, nxtParams)
			ugRef
		})
	}
	def moveAllGoodies(ovwc : OffersVWorldClient, dynaParamID : Ident, dynaDelta : Float, durSec : Float, delayMsec_opt : Option[Int]) : Unit = {
		myRefs.foreach( ugref => {
			val lastParams = ugref.getLastParams
			val nxtParams = duplicateParams(lastParams)
			adjustFloatParam(nxtParams, dynaParamID, dynaDelta)
			setDurParam(nxtParams, durSec)
			val taRq = ugref.makeReqAndUpdate(GoodyNames.ACTION_MOVE, nxtParams)
			ovwc.sendTARq(taRq)
			delayAsNeeded(delayMsec_opt)
		})
	}
	def deleteAllGoodies(ovwc : OffersVWorldClient, delayMsec_opt : Option[Int]) : Unit = {
		myRefs.foreach(ugref => {
			val taRq = ugref.makeDeleteReq
			ovwc.sendTARq(taRq)
			delayAsNeeded(delayMsec_opt)
		})
	}

}
class AnotherBurstTest(ovwc : OffersVWorldClient) extends Hey with VarargsLogging {

	val emptyParams = new ConcreteTVM
	val seedLocParam = makeLoc3Params(-10.0f, 15.0f, -2.0f)

	val stateFalse = makeBoolStateParam(false)
	val stateTrue = makeBoolStateParam(true)

	val stateX = makeXOStateParam(false)
	val stateO = makeXOStateParam(true)
	// alpha=0.0f is transparent,  1.0f is opaque
	val redTrans = makeColorParam(1.0f, 0.0f, 0.0f, 0.6f)

	val turnAboutY_45deg = makeRotParam(45.0f, 0.0f, 1.0f, 0.0f)

	def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap) : Unit = {

		val burstParams = combineParams(List(seedLocParam, topParams))

		val brst = new OneBurst(bName, 12, 16)

		brst.createGoodies(ovwc, goodyTypeID, burstParams, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

		brst.deleteAllGoodies(ovwc, Some(300))
	}

	def fireHorizBursts() : Unit = {
		val boxParams = combineParams(List(redTrans, turnAboutY_45deg))
		fireHorizBurst("horizBoxOne", GoodyNames.TYPE_BOX, boxParams)
	}
	def fireSomeBursts(): Unit = {
		fireHorizBursts()
	}
	def fireLesserBursts(): Unit = {

		val b1Params = combineParams(List(seedLocParam, stateX))

		val brst1 = new OneBurst("brstOne", 12, 16)

		brst1.createGoodies(ovwc, GoodyNames.TYPE_TICTAC_MARK, b1Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst1.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

		val b2Params = combineParams(List(seedLocParam, stateFalse))

		adjustFloatParam(b2Params,  GoodyNames.LOCATION_Y, 3.0f)

		val brst2 = new OneBurst("brstTwo", 12, 16)

		brst2.createGoodies(ovwc, GoodyNames.TYPE_BIT_BOX, b2Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst2.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

		brst1.deleteAllGoodies(ovwc, Some(300))

		brst2.deleteAllGoodies(ovwc, Some(300))
	}

}