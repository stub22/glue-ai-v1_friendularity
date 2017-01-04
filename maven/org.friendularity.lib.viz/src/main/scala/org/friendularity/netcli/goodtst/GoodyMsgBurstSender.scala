package org.friendularity.netcli.goodtst

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.SerTypedValueMap
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.{OffersVWorldClient, VWTAMsgMaker}
import org.friendularity.vwimpl.IdentHlp

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
// burstLen = not yet used, but intended to be the number of incremental moves in a sequence
class GoodyMsgBurstSender(entIdPrfx : String, burstWidth : Int, burstLen : Int) extends IdentBuncher {
	lazy val myIdents : List[Ident] = makeSomeUniqueIdents(burstWidth, entIdPrfx)

	var myRefs : List[GoodyClientState] = Nil

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
			val ugRef = new GoodyClientState(goodyID, typeID, taRq, nxtParams)
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