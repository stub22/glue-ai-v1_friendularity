package org.friendularity.netcli.goodtst

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.SerTypedValueMap
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.{OffersVWorldClient, VWTAMsgMaker}
import org.friendularity.util.IdentHlp

/**
  * Created by Owner on 1/4/2017.
  */
trait IdentBuncher extends IdentHlp with GoodyParamMaker with VWTAMsgMaker{
	def makeSomeUniqueIdents(idCount : Int, nickPrefix : String) : List[Ident] = {
		var idx = 0

		val indices : Range = 0 to (idCount - 1)
		// TODO:  Format digit string with leading 0s
		val nicks = indices.map(nickPrefix + _)
		val idents = nicks.map(makeStampyRandyIdent(_)).toList
		idents
	}
}

// burstWidth = num goodies
// burstLen = not yet used, but intended to be the dflt number of incremental moves in a gen seq
class GoodyMsgBurstSender(entIdPrfx : String, burstWidth : Int, burstLen : Int) extends IdentBuncher {
	lazy val myIdents : List[Ident] = makeSomeUniqueIdents(burstWidth, entIdPrfx)

	var myRefs : List[GoodyClientState] = Nil

	def pauseAsNeeded(delayMsec_opt : Option[Int]) : Unit = {
		if (delayMsec_opt.isDefined) {
			Thread.sleep(delayMsec_opt.get)
		}
	}
	// N
	// TODO:  Wrap up dyna pairs in a class, then pass a collection
	def createGoodies(ovwc : OffersVWorldClient, typeID : Ident, seedParams : SerTypedValueMap,
					  dynaParamID : Ident, dynaDelta : Float, pauseMsec_opt : Option[Int]) : Unit = {
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
			pauseAsNeeded(pauseMsec_opt)
			val ugRef = new GoodyClientState(goodyID, typeID, taRq, nxtParams)
			ugRef
		})
	}
	def moveAllGoodies(ovwc : OffersVWorldClient, dynaParamID : Ident, dynaDelta : Float,
					   rotParam_opt : Option[SerTypedValueMap], durSec : Float,
					   pauseMsec_opt : Option[Int]) : Unit = {
		myRefs.foreach( ugref => {
			// Unreliable, uses too many assumptions about state of last params.
			val initParams  = ugref.getInitParams
			val nxtParams = duplicateParams(initParams)
			adjustFloatParam(nxtParams, dynaParamID, dynaDelta)
			setDurParam(nxtParams, durSec)
			val actualParams = combineParams(List(nxtParams) ::: rotParam_opt.toList)
			val taRq = ugref.makeReqAndUpdate(GoodyNames.ACTION_MOVE, actualParams)
			ovwc.sendTARq(taRq)
			pauseAsNeeded(pauseMsec_opt)
		})
	}
	def deleteAllGoodies(ovwc : OffersVWorldClient, pauseMsec_opt : Option[Int]) : Unit = {
		myRefs.foreach(ugref => {
			val taRq = ugref.makeDeleteReq
			ovwc.sendTARq(taRq)
			pauseAsNeeded(pauseMsec_opt)
		})
	}
	def setAllGoodies(ovwc : OffersVWorldClient, paramsFunc : Function1[Int, SerTypedValueMap],
					  pauseMsec_opt : Option[Int]) : Unit = {
		var idx = 0
		myRefs.foreach( ugref => {
			val nxtParams = paramsFunc(idx)
			val taRq = ugref.makeReqAndUpdate(GoodyNames.ACTION_SET, nxtParams)
			ovwc.sendTARq(taRq)
			pauseAsNeeded(pauseMsec_opt)
			idx += 1
		})
	}

}