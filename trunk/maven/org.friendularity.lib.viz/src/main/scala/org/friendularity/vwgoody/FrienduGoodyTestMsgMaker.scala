package org.friendularity.vwgoody

import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{ThingActionSpec, SerTypedValueMap}
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.{BasicTypedValueMap, BasicThingActionSpec}
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.dir.NamespaceDir
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.{OffersVWorldClient, VWTAMsgMaker}

/**
  * Created by Owner on 12/15/2016.
  */
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
		val idxes = 0 to (gridCount - 1)

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
