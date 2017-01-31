package org.friendularity.netcli.goodtst

import com.jme3.math.Quaternion
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.SerTypedValueMap
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.goody.GoodyNames
import org.friendularity.netcli.vwta.OffersVWorldClient

/**
  * Created by Stub22 on 1/4/2017.
  */
class TestManyGoodyBursts(ovwc : OffersVWorldClient) extends GoodyParamMaker with VarargsLogging with GoodyTypeAliases {

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

	val degRot = 90f
	val rotUpParam = makeRotParam(degRot, 1f, 0f, 0f)


	val noSetParamsFunc :  Option[Function1[Int, SerTypedValueMap]] = None

	val noMoveRotParam :  Option[SerTypedValueMap] = None
	val wackyMoveRotParam = makeRotParam(80.0f, 0.2f, 1.0f, 0.4f)

	val DFL_X_SPC = 2.5f
	val DFL_Y_DELT = 1.3f
	val DFL_STP_DLY_OPT = Some(300)

	//private val ROTATE_UPRIGHT_EULER : Array[Float] = Array((Math.PI / 2).toFloat, 0f, 0f)
	//private val rotUpQuat = new Quaternion(ROTATE_UPRIGHT_EULER)


	def fireHorizBurst_withWackyMoveRot(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap) : Unit = {

		fireHorizBurst(bName, goodyTypeID, topParams, DFL_X_SPC, DFL_Y_DELT, noSetParamsFunc, Some(wackyMoveRotParam), DFL_STP_DLY_OPT)
	}

	def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap) : Unit = {

		fireHorizBurst(bName, goodyTypeID, topParams, DFL_X_SPC, DFL_Y_DELT, noSetParamsFunc, noMoveRotParam, DFL_STP_DLY_OPT)
	}

	def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap, xSpacing : Float,
					   yDelta: Float, setParmsFunc_opt : Option[Function1[Int, SerTypedValueMap]],
					   moveRotParam_opt : Option[SerTypedValueMap], stepDelay_opt : Option[Int]) : Unit = {

		val burstParams = combineParams(List(seedLocParam, topParams))

		val brst = new GoodyMsgBurstSender(bName, 12, 16)

		brst.createGoodies(ovwc, goodyTypeID, burstParams, GoodyNames.LOCATION_X, xSpacing, stepDelay_opt)

		// If user supplies a set-func, we apply it across all the goodies.
		if (setParmsFunc_opt.isDefined) {
			brst.setAllGoodies(ovwc, setParmsFunc_opt.get, stepDelay_opt)
		}

		brst.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, yDelta, moveRotParam_opt, 2.5f, stepDelay_opt)

		brst.deleteAllGoodies(ovwc, stepDelay_opt)
	}

	def fireBoxBurst() : Unit = {
		// BOX type *requires* that we set a rotation or it throws nullies.
		val yPos = makeFloatParam(GoodyNames.LOCATION_Y, 6.0f)

		val boxParams = combineParams(List(redTrans, turnAboutY_45deg, yPos))

		fireHorizBurst_withWackyMoveRot("horizBoxOne", GT_BOX, boxParams)
	}

	def fireBitCubeBurst() : Unit = {
		// TODO:  BitCube expects these texture images:
		//		Texture zeroTexture = assetMgr.loadTexture("textures/robosteps/Zero.png");
		//		Texture oneTexture = assetMgr.loadTexture("textures/robosteps/One.png");
		//		Texture blankTexture = assetMgr.loadTexture("textures/robosteps/BlankGray.png");
		val yPos = makeFloatParam(GoodyNames.LOCATION_Y, 8.0f)

		val bcubeParams = combineParams(List(redTrans, turnAboutY_45deg, stateTrue, yPos))

		fireHorizBurst_withWackyMoveRot("horizBitCubeOne", GT_BIT_CUBE, bcubeParams)
	}

	private val colorChoiceParams = List(grnTrans, prpTrans, ylwTrans)
	private def ticTacStateSetParmsFunc(idx : Int) : SerTypedValueMap = {
		val isOparm = makeXOStateParam(idx % 2 == 1) // idx-even => false = "X", idx-odd => true = "O"
		val hPos = (idx % 3) + 1   // ranges over 1,2,3
		val vPos = ((idx / 3) % 3) + 1   // ranges over 1,2,3
		val hParam = makeIntParam(GoodyNames.COORDINATE_X, hPos)
		val vParam = makeIntParam(GoodyNames.COORDINATE_Y, vPos)
		val colorParam = colorChoiceParams(hPos - 1)

		combineParams(List(isOparm, hParam, vParam, colorParam))
	}
	def fireTicTacGridBurst() : Unit = {

		val posParams = makeLoc3Params(-20.0f, 15.0f, -8.0f)

		// radIsNotUsed

		val ttGridParams = combineParams(List(redTrans, rotUpParam,  posParams)) // turnAboutY_45deg

		fireHorizBurst("horizTTGridOne", GT_TICTAC_GRID, ttGridParams, DFL_X_SPC * 4.0f, DFL_Y_DELT * 3.0f,
					Some(ticTacStateSetParmsFunc), Some(wackyMoveRotParam), Some(700))
	}
	def fireTicXBurst() : Unit = {
		// TicX is in the X-Z plane by default, so we rotate it up for display.
		val posParams = makeLoc3Params(9.0f, 11.0f, -2.0f)
		val isOparm = makeXOStateParam(false)
		val ticXparms = combineParams(List(prpTrans, posParams, rotUpParam, isOparm))
		fireHorizBurst_withWackyMoveRot("ticX", GT_TICTAC_MARK, ticXparms)
	}
	def fireTacOBurst() : Unit = {
		// TicY is in the X-Z plane by default (after its own internal rotation), so we rotate it up for display.
		val posParams = makeLoc3Params(7.0f, 3.0f, 2.0f)
		val isOparm = makeXOStateParam(true)
		val ticOparms = combineParams(List(ylwTrans, posParams, rotUpParam, isOparm))
		fireHorizBurst_withWackyMoveRot("tacO", GT_TICTAC_MARK, ticOparms)
	}

	def fireFloorBurst() : Unit = {
		val posParams = makeLoc3Params(-5.0f, 1.5f, -2.0f)

		val floorParams = combineParams(List(bluTrans, turnAboutZ_45deg, posParams))

		fireHorizBurst("horizFloorOne", GT_FLOOR, floorParams, 6.0f, 3.0f, noSetParamsFunc, Some(wackyMoveRotParam),  Some(700))
	}

	def fireTextBurst() : Unit = {
		val locParams = makeLoc3Params(150.0f, 300.0f, 0.5f)
		val txtParam = makeTxtParam("Burst o Text is HERE")
		val scale3 = makeUniformScaleParam(1.0f)
		val prmsCombined = combineParams(List(txtParam, scale3, locParams))
		fireHorizBurst("horizTxt", GT_TEXT, prmsCombined, 20.0f, 40.0f, noSetParamsFunc, noMoveRotParam, Some(500))
	}
	def fireScoreboardBurst() : Unit = {
		/*
		default-dispatcher-5] org.friendularity.vwgoody.BetterBGC (BetterBGC.scala:107) createByAction - Scoreboard row count=4, rowHeight=10.0, textSize=4.0, locVec=(220.0, 220.0, 0.5)
		default-dispatcher-3] org.appdapter.api.facade.MakableObjectHelpFuncs (MakableObjectHelpFuncs.java:67) makeObj - Making new object named CC_SCENE_FLAT_FACADE using default constructor of class org.cogchar.render.opengl.scene.FlatOverlayMgr
		default-dispatcher-3] org.appdapter.api.facade.MakableObjectHelpFuncs (MakableObjectHelpFuncs.java:67) makeObj - Making new object named CC_SCENE_TEXT_FACADE using default constructor of class org.cogchar.render.opengl.scene.TextMgr
		default-dispatcher-3] org.cogchar.render.opengl.scene.TextMgr (TextMgr.java:67) disableCullingForFont - TextMgr disabling culling for a total of 1  font materials
		 */
		val locParams = makeLoc3Params(200.0f, 220.0f, 0.5f)
		val scale = makeUniformScaleParam(4.0f)         // = textSize
		val sizes = makeSize3Params(10.0f, 10.0f, 10.0f) // = rowHeight
		val rows = makeRowCountParam(4)
		val sbParams = combineParams(List(scale, locParams, sizes, rows))

		fireHorizBurst("horizScore", GT_SCOREBOARD, sbParams, 20.0f, 30.0f, noSetParamsFunc, noMoveRotParam, Some(400))
	}
	def fireCrosshairBurst() : Unit = {
		//  org.cogchar.render.goody.flat.CrossHairGoody (FlatGoodyWithScreenFracPos.java:41)
		// <init> - Cannot find screen dimension.
		val locParams = makeLoc3Params(0.1f, 0.5f, 0.0f)
		val scale = makeUniformScaleParam(1.0f)
		val chParams = combineParams(List(scale, locParams))
		fireHorizBurst("horizCross", GT_CROSSHAIR, chParams, 0.1f, 0.1f, noSetParamsFunc, noMoveRotParam, Some(400))
	}
	def fireSomeBursts(): Unit = {

		fireTacOBurst()

		fireBitCubeBurst()

		fireTicXBurst()

		fireTicTacGridBurst()

		fireTextBurst()

		fireFloorBurst()

		fireBoxBurst()

		// Above goodies are used by RK STEM as of 2017-Jan, below are not.
//		fireScoreboardBurst()
//		fireCrosshairBurst()

//		overlapSomeBursts()
	}
	def overlapSomeBursts(): Unit = {

		val b1Params = combineParams(List(seedLocParam, stateX))

		val brst1 = new GoodyMsgBurstSender("brstOne", 12, 16)

		brst1.createGoodies(ovwc, GT_TICTAC_MARK, b1Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst1.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, noMoveRotParam,  2.5f, Some(300))

		val b2Params = combineParams(List(seedLocParam, stateFalse))

		adjustFloatParam(b2Params,  GoodyNames.LOCATION_Y, 3.0f)

		val brst2 = new GoodyMsgBurstSender("brstTwo", 12, 16)

		brst2.createGoodies(ovwc, GT_BIT_BOX, b2Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

		brst2.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, noMoveRotParam, 2.5f, Some(300))

		brst1.deleteAllGoodies(ovwc, Some(300))

		brst2.deleteAllGoodies(ovwc, Some(300))
	}

}