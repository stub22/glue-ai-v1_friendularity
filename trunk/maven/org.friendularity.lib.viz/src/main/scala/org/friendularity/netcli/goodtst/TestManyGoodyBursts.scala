package org.friendularity.netcli.goodtst

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


		def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap) : Unit = {
			val (xSpc, yDelt, stepDlyMsec_opt) = (2.5f, 1.3f, Some(300))
			fireHorizBurst(bName, goodyTypeID, topParams, xSpc, yDelt, stepDlyMsec_opt)
		}

		def fireHorizBurst(bName : String, goodyTypeID : Ident, topParams : SerTypedValueMap, xSpacing : Float,
						   yDelta: Float, stepDelay_opt : Option[Int]) : Unit = {

			val burstParams = combineParams(List(seedLocParam, topParams))

			val brst = new GoodyMsgBurstSender(bName, 12, 16)

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

			fireHorizBurst("horizScore", GT_SCOREBOARD, sbParams, 20.0f, 30.0f, Some(400))
		}
		def fireCrosshairBurst() : Unit = {
			//  org.cogchar.render.goody.flat.CrossHairGoody (FlatGoodyWithScreenFracPos.java:41)
			// <init> - Cannot find screen dimension.
			val locParams = makeLoc3Params(0.1f, 0.5f, 0.0f)
			val scale = makeUniformScaleParam(1.0f)
			val chParams = combineParams(List(scale, locParams))
			fireHorizBurst("horizCross", GT_CROSSHAIR, chParams, 0.1f, 0.1f, Some(400))
		}
		def fireSomeBursts(): Unit = {
			fireTicTacGridBurst()
			fireBoxBurst()

			fireFloorBurst()
			fireTextBurst()
			fireScoreboardBurst()
			fireCrosshairBurst()

			// fireBitCubeBurst()

			overlapSomeBursts()
		}
		def overlapSomeBursts(): Unit = {

			val b1Params = combineParams(List(seedLocParam, stateX))

			val brst1 = new GoodyMsgBurstSender("brstOne", 12, 16)

			brst1.createGoodies(ovwc, GT_TICTAC_MARK, b1Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

			brst1.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

			val b2Params = combineParams(List(seedLocParam, stateFalse))

			adjustFloatParam(b2Params,  GoodyNames.LOCATION_Y, 3.0f)

			val brst2 = new GoodyMsgBurstSender("brstTwo", 12, 16)

			brst2.createGoodies(ovwc, GT_BIT_BOX, b2Params, GoodyNames.LOCATION_X, 2.5f, Some(300))

			brst2.moveAllGoodies(ovwc, GoodyNames.LOCATION_Y, 1.3f, 2.5f, Some(300))

			brst1.deleteAllGoodies(ovwc, Some(300))

			brst2.deleteAllGoodies(ovwc, Some(300))
		}

	}