package org.friendularity.netcli.goodtst

import java.lang.{Boolean => JBool, Float => JFloat, Integer => JInt}

import com.jme3.math.ColorRGBA
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{SerTypedValueMap, ThingActionSpec, TypedValueMap}
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.BasicTypedValueMap
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.VWTAMsgMaker

/**
  * Created by Stub22 on 12/26/2016.
  *
  * So far this code is used only for outer client test-msg creation.
  */
trait GoodyParamMaker extends VWTAMsgMaker with VarargsLogging {
	// entity-ID is a bookeeping handle, of fixed type
	// type and verb help define meaning of params
	// params are leaf data describing an initial or updated (partial) state, except on verb=delete

	/*
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
	*/

	def makeLoc3Params (x : Float, y: Float, z: Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putLocation(x, y, z)
		btvm
	}

	def makeUniformScaleParam(uniScale : Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putScaleUniform(uniScale)
		btvm
	}
	def makeSize3Params(xSize : Float, ySize: Float, zSize : Float) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putSize(xSize, ySize, zSize)
		btvm
	}
	// delta is added to existing param, or
	def adjustFloatParam(tgtParams : TypedValueMap, paramID : Ident, delta : Float) : Unit = {
		val oldVal_orNull : JFloat = tgtParams.getAsFloat(paramID)
		val oldVal : Float = if (oldVal_orNull == null) 0.0f else oldVal_orNull
		var nwVal = oldVal + delta
		tgtParams.putValueAtName(paramID, nwVal)
	}
	def makeIntParam(paramID: Ident, pval : Int) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putObjectAtName(paramID, pval)
		btvm
	}
	def makeFloatParam(paramID: Ident, pval : Float): SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putObjectAtName(paramID, pval)
		btvm
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
	def makeColorParam(clr : ColorRGBA) : SerTypedValueMap = {
		makeColorParam(clr.r, clr.g, clr.b, clr.a)
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
	def makeTxtParam(txtVal : String): SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putObjectAtName(GoodyNames.TEXT, txtVal)
		btvm
	}
	def makeRowCountParam(rowCount : Int) : SerTypedValueMap = {
		val btvm : BasicTypedValueMap  = new ConcreteTVM()
		val paramWriter = new GoodyActionParamWriter(btvm)
		paramWriter.putObjectAtName(GoodyNames.ROWS, rowCount)
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

