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
trait Buncher extends IdentHlp with GoodyParamMaker with VWTAMsgMaker{
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
trait GoodyTypeAliases {
	val GT_BOX = GoodyNames.TYPE_BOX
	val GT_BIT_CUBE = GoodyNames.TYPE_BIT_CUBE
	val GT_BIT_BOX = GoodyNames.TYPE_BIT_BOX
	val GT_TICTAC_GRID = GoodyNames.TYPE_TICTAC_GRID
	val GT_TICTAC_MARK = GoodyNames.TYPE_TICTAC_MARK

	val GT_FLOOR = GoodyNames.TYPE_FLOOR

	val GT_CROSSHAIR = GoodyNames.TYPE_CROSSHAIR
	val GT_TEXT  = GoodyNames.TYPE_TEXT
	val GT_SCOREBOARD = GoodyNames.TYPE_SCOREBOARD

	val GT_AVATAR = GoodyNames.TYPE_AVATAR
	val GT_CAMERA = GoodyNames.TYPE_CAMERA

}
class AnotherBurstTest(ovwc : OffersVWorldClient) extends GoodyParamMaker with VarargsLogging with GoodyTypeAliases {

	val emptyParams = new ConcreteTVM

	val (seedLocX, seedLocY, seedLocZ) = (-10.0f, 18.0f, -2.0f)
	val seedLocParam = makeLoc3Params(seedLocX, seedLocY, seedLocZ)

	val stateFalse = makeBoolStateParam(false)
	val stateTrue = makeBoolStateParam(true)

	val stateX = makeXOStateParam(false)
	val stateO = makeXOStateParam(true)

	// alpha=0.0f is transparent,  1.0f is opaque
	val redTrans = makeColorParam(1.0f, 0.0f, 0.0f, 0.4f)
	val bluTrans = makeColorParam(0.0f, 0.0f, 1.0f, 0.4f)
	val grnTrans = makeColorParam(0.0f, 1.0f, 0.0f, 0.4f)
	val ylwTrans = makeColorParam(1.0f, 1.0f, 0.0f, 0.4f)
	val prpTrans = makeColorParam(1.0f, 0.0f, 1.0f, 0.4f)

	val turnAboutY_45deg = makeRotParam(45.0f, 0.0f, 1.0f, 0.0f)
	val turnAboutZ_45deg = makeRotParam(45.0f, 0.0f, 0.0f, 1.0f)
	val turnAboutX_45deg = makeRotParam(45.0f, 1.0f, 0.0f, 0.0f)


	def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap) : Unit = {
		fireHorizBurst(bName, goodyTypeID, topParams, 2.5f, 1.3f, Some(300))
	}

	def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap, xSpacing : Float,
					   yDelta: Float, stepDelay_opt : Option[Int]) : Unit = {

		val burstParams = combineParams(List(seedLocParam, topParams))

		val brst = new OneBurst(bName, 12, 16)

		brst.createGoodies(ovwc, goodyTypeID, burstParams, GoodyNames.LOCATION_X, xSpacing, stepDelay_opt)

		brst.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, yDelta, 2.5f, stepDelay_opt)

		brst.deleteAllGoodies(ovwc, stepDelay_opt)
	}

	def fireBoxBurst() : Unit = {
		// BOX type *requires* that we set a rotation or it throws nullies.
		val yPos = makeFloatParam(GoodyNames.LOCATION_Y, 6.0f)

		val boxParams = combineParams(List(redTrans, turnAboutY_45deg, yPos))

		fireHorizBurst("horizBoxOne", GT_BOX, boxParams)
	}

	def fireBitCubeBurst() : Unit = {
		// TODO:  BitCube expects these texture images:
//		Texture zeroTexture = assetMgr.loadTexture("textures/robosteps/Zero.png");
//		Texture oneTexture = assetMgr.loadTexture("textures/robosteps/One.png");
//		Texture blankTexture = assetMgr.loadTexture("textures/robosteps/BlankGray.png");
		val yPos = makeFloatParam(GoodyNames.LOCATION_Y, 8.0f)

		val boxParams = combineParams(List(redTrans, turnAboutY_45deg, stateTrue, yPos))

		fireHorizBurst("horizBitCubeOne", GT_BIT_CUBE, boxParams)
	}
	def fireTicTacGridBurst() : Unit = {
		val zPos = makeFloatParam(GoodyNames.LOCATION_Z, -10.0f)

		val boxParams = combineParams(List(redTrans, turnAboutY_45deg,  zPos))

		fireHorizBurst("horizBitCubeOne", GT_TICTAC_GRID, boxParams)
	}
	def fireFloorBurst() : Unit = {
		val yPos = makeFloatParam(GoodyNames.LOCATION_Y, -3.0f)

		val floorParams = combineParams(List(bluTrans, turnAboutZ_45deg, yPos))

		fireHorizBurst("horizFloorOne", GT_FLOOR, floorParams, 6.0f, 3.0f, Some(700))

	}

	def fireTextBurst() : Unit = {
		val locParams = makeLoc3Params(150.0f, 300.0f, 0.5f)
		val txtParam = makeTxtParam("Burst o Text is HERE")
		val scale3 = makeUniformScaleParam(1.0f)
		val txtParams = combineParams(List(txtParam, scale3, locParams))
		fireHorizBurst("horizTxt", GT_TEXT, txtParams, 20.0f, 40.0f, Some(500))
	}
	def fireScoreboardBurst() : Unit = {
/*
default-dispatcher-3] org.friendularity.vwgoody.VWGoodyActor (HasLogger.scala:33) info4 - VWGoodyJobLogic is processing received actSpec of class=class org.cogchar.impl.thing.basic.BasicThingActionSpec, verb=FreeIdent[absUri=urn:ftd:cogchar.org:2012:goody#ActionCreate], tgtType=FreeIdent[absUri=urn:ftd:cogchar.org:2012:goody#ScoreBoard] tgtID=FreeIdent[absUri=urn:sri_horizScore0_1482796133113_829162#id]
default-dispatcher-3] org.friendularity.vwgoody.BetterBGC (BasicGoodyCtxImpl.java:216) consumeAction - The targetThingType is FreeIdent[absUri=urn:ftd:cogchar.org:2012:goody#ScoreBoard]
default-dispatcher-3] org.friendularity.vwgoody.BetterBGC (BasicGoodyCtxImpl.java:225) consumeAction - The kind of Goody inspected is CREATE
default-dispatcher-3] org.friendularity.vwgoody.BetterBGC$$anon$1 (HasLogger.scala:30) info1 - BetterBGC seeking match for goodyType=FreeIdent[absUri=urn:ftd:cogchar.org:2012:goody#ScoreBoard]
default-dispatcher-5] org.friendularity.vwgoody.BetterBGC (BetterBGC.scala:107) createByAction - Scoreboard row count=4, rowHeight=10.0, textSize=4.0, locVec=(220.0, 220.0, 0.5)

default-dispatcher-3] org.appdapter.api.facade.MakableObjectHelpFuncs (MakableObjectHelpFuncs.java:67) makeObj - Making new object named CC_SCENE_FLAT_FACADE using default constructor of class org.cogchar.render.opengl.scene.FlatOverlayMgr
default-dispatcher-3] org.appdapter.api.facade.MakableObjectHelpFuncs (MakableObjectHelpFuncs.java:67) makeObj - Making new object named CC_SCENE_TEXT_FACADE using default constructor of class org.cogchar.render.opengl.scene.TextMgr
default-dispatcher-3] org.cogchar.render.opengl.scene.TextMgr (TextMgr.java:67) disableCullingForFont - TextMgr disabling culling for a total of 1  font materials
default-dispatcher-3] org.cogchar.render.app.entity.VWorldEntityReg (VWorldEntityReg.java:20) addGoody - Adding Goody with URI: FreeIdent[absUri=urn:sri_horizScore0_1482796133113_829162#id]
 */
		val locParams = makeLoc3Params(200.0f, 220.0f, 0.5f)
		val scale = makeUniformScaleParam(4.0f)         // = textSize
		val sizes = makeSize3Params(10.0f, 10.0f, 10.0f) // = rowHeight
		val rows = makeRowCountParam(4)
		val sbParams = combineParams(List(scale, locParams, sizes, rows))

		fireHorizBurst("horizScore", GT_SCOREBOARD, sbParams, 20.0f, 30.0f, Some(500))
	}
	def fireCrosshairBurst() : Unit = {
		//  org.cogchar.render.goody.flat.CrossHairGoody (FlatGoodyWithScreenFracPos.java:41)
		// <init> - Cannot find screen dimension.
		val locParams = makeLoc3Params(0.1f, 0.5f, 0.0f)
		val scale = makeUniformScaleParam(1.0f)
		val chParams = combineParams(List(scale, locParams))
		fireHorizBurst("horizCross", GT_CROSSHAIR, chParams, 0.1f, 0.1f, Some(500))
	}
	def fireSomeBursts(): Unit = {
		fireFloorBurst()
		fireTextBurst()
		fireScoreboardBurst()
		fireCrosshairBurst()
		fireBoxBurst()
		// fireBitCubeBurst()
		fireTicTacGridBurst()
		overlapSomeBursts()
	}
	def overlapSomeBursts(): Unit = {

		val b1Params = combineParams(List(seedLocParam, stateX))

		val brst1 = new OneBurst("brstOne", 12, 16)

		brst1.createGoodies(ovwc, GT_TICTAC_MARK, b1Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst1.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

		val b2Params = combineParams(List(seedLocParam, stateFalse))

		adjustFloatParam(b2Params,  GoodyNames.LOCATION_Y, 3.0f)

		val brst2 = new OneBurst("brstTwo", 12, 16)

		brst2.createGoodies(ovwc, GT_BIT_BOX, b2Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst2.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

		brst1.deleteAllGoodies(ovwc, Some(300))

		brst2.deleteAllGoodies(ovwc, Some(300))
	}

}