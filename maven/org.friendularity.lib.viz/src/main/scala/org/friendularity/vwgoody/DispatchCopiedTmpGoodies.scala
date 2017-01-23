package org.friendularity.vwgoody

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}

import org.appdapter.core.name.Ident

import org.cogchar.name.goody.GoodyNames

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.app.entity.{VWorldEntity, GoodyActionExtractor}
import org.cogchar.render.goody.basic.BasicGoodyCtx

import org.friendularity.tmpgood.tgothr.{TG_VirtualFloor, TG_GoodyBox}
import org.friendularity.tmpgood.tgbit.{TG_BitCube, TG_BitBox}
import org.friendularity.tmpgood.tgflat.{TG_ParagraphGoody, TG_ScoreBoardGoody, TG_CrossHairGoody}
import org.friendularity.tmpgood.tgtic.{TG_TicTacGrid, TG_TicTacMark}

/**
  * Created by Stub22 on 1/2/2017.
  *
  */
trait DispatchCopiedTmpGoodies extends GoodyParamExtractorHelp with BasicGoodyCtx {
	private val myVargler = new VarargsLogging {}
	protected def createByAction_TG_tmpCopies2017(ga: GoodyActionExtractor): VWorldEntity = {

		var novGoody: VWorldEntity = null

		if (ga.getKind eq GoodyActionExtractor.Kind.CREATE) {
			try {
				val goodyID: Ident = ga.getGoodyID
				val goodyType: Ident = ga.getType
				val locVec: Vector3f = ga.getLocationVec3f

				val bgc: BasicGoodyCtx = this
				myVargler.info1("DispatchCopiedTmpGoodies seeking match for goodyType={}", goodyType)
				if (GoodyNames.TYPE_BIT_BOX == goodyType) {
					val bitBoxState: Boolean = ga.getSpecialBoolean(GoodyNames.BOOLEAN_STATE)
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					novGoody = new TG_BitBox(bgc, goodyID, locVec, rotQuat, scaleVec, bitBoxState)
				}
				else if (GoodyNames.TYPE_BIT_CUBE == goodyType) {
					val bitBoxState: Boolean = ga.getSpecialBoolean(GoodyNames.BOOLEAN_STATE)
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					novGoody = new TG_BitCube(bgc, goodyID, locVec, rotQuat, scaleVec, bitBoxState)
				}
				else if (GoodyNames.TYPE_FLOOR == goodyType) {
					val gcolor: ColorRGBA = ga.getColor
					novGoody = new TG_VirtualFloor(bgc, goodyID, locVec, gcolor, true)
				}
				else if (GoodyNames.TYPE_TICTAC_MARK == goodyType) {
					val isAnO: Boolean = ga.getSpecialBoolean(GoodyNames.USE_O)
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					novGoody = new TG_TicTacMark(bgc, goodyID, locVec, rotQuat, scaleVec, isAnO)
				}
				else if (GoodyNames.TYPE_TICTAC_GRID == goodyType) {
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColor
					novGoody = new TG_TicTacGrid(bgc, goodyID, locVec, rotQuat, gcolor, scaleVec)
				}
				else if (GoodyNames.TYPE_BOX == goodyType) {
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColor
					novGoody = new TG_GoodyBox(bgc, goodyID, locVec, rotQuat, gcolor, scaleVec)
				}
				else if (GoodyNames.TYPE_CROSSHAIR == goodyType) {
					val scaleUniform: java.lang.Float =  getScaleUniformFrom(ga)
					novGoody = new TG_CrossHairGoody(bgc, goodyID, locVec, scaleUniform)
				}
				else if (GoodyNames.TYPE_SCOREBOARD == goodyType) {
					val rowCount: Integer = ga.getSpecialInteger(GoodyNames.ROWS)
					val sizeVec: Array[java.lang.Float] = ga.getSizeVec3D
					val sizeX: Float = sizeVec(0)
					val rowHeight: Float = sizeX
					val scaleUniform: java.lang.Float =  getScaleUniformFrom(ga)
					val textSize: Float = scaleUniform
		//			myVargler.info("Scoreboard row count=" + rowCount + ", rowHeight=" + rowHeight + ", textSize=" + textSize + ", locVec=" + locVec)
					myVargler.info4("Scoreboard row count={}, textSize={}, rowHeight={}, locVec={}", rowCount, rowHeight : java.lang.Float, textSize : java.lang.Float, locVec)
					novGoody = new TG_ScoreBoardGoody(bgc, goodyID, locVec, rowHeight, rowCount, textSize)
				}
				else if (GoodyNames.TYPE_TEXT == goodyType) {
					val goodyText: String = ga.getText
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColor
					novGoody = new TG_ParagraphGoody(bgc, goodyID, locVec, scaleVec.getX, gcolor, goodyText)
				}
				else {
					myVargler.warn1("Did not recognize requested goody type for creation: {}", ga.getType)
				}
			}
			catch {
				case e: Exception => {
					myVargler.error2("Error attempting to create goody {}", ga.getGoodyID, e)
				}
			}
		}
		else {
			myVargler.warn1("GoodyFactory received request to add a goody, but the GoodyAction kind was not CREATE! Goody URI: {}", ga.getGoodyID)
		}
		novGoody

	}

}
