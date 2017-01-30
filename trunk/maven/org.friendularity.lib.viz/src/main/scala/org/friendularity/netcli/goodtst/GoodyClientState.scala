package org.friendularity.netcli.goodtst

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{SerTypedValueMap, ThingActionSpec}
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.VWTAMsgMaker

/**
  * Created by Owner on 1/4/2017.
  */

// Client-side handle for a single goody, keeps track of lastParams made.
class GoodyClientState(goodyID : Ident, typeID : Ident, initTASpec : ThingActionSpec,
					   initParams : SerTypedValueMap) extends VWTAMsgMaker {

	private var		myLastParams : SerTypedValueMap = initParams
	private var		myLastTASpec : ThingActionSpec = initTASpec

	def getInitParams : SerTypedValueMap = initParams
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
