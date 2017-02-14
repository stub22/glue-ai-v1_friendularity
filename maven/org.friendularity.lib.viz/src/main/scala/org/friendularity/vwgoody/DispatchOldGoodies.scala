package org.friendularity.vwgoody

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.{VWorldEntity, GoodyActionExtractor}
import org.cogchar.render.goody.basic.BasicGoodyCtx

/**
  * Created by Owner on 1/2/2017.
  */
trait DispatchOldGoodies extends GoodyParamExtractorHelp with BasicGoodyCtx {
	private val myVargler = new VarargsLogging {}

	/**
	  * Changed this method to unpack variables only if they are used by the current
	  *  {@link VWorldEntity}. If we don't we get nullPointerExceptions when trying to apply a type
	  * to null values.
	  * - Ben[2016-09-29]
	  */
	protected def createByAction_emulate2014(ga: GoodyActionExtractor): VWorldEntity = {

		import org.cogchar.render.goody.basic.{GoodyBox, VirtualFloor, BasicGoodyCtx, BasicGoodyCtxImpl}
		import org.cogchar.render.goody.bit.{TicTacGrid, TicTacMark, BitCube, BitBox}
		import org.cogchar.render.goody.flat.{ParagraphGoody, ScoreBoardGoody, CrossHairGoody}

		var novGoody: VWorldEntity = null

		if (ga.getKind eq GoodyActionExtractor.Kind.CREATE) {
			try {
				val goodyID: Ident = ga.getGoodyID
				val goodyType: Ident = ga.getType
				val locVec: Vector3f = ga.getLocationVec3f

				val bgc: BasicGoodyCtx = this
				myVargler.info1("DispatchOldGoodies seeking match for goodyType={}", goodyType)
				if (GoodyNames.TYPE_BIT_BOX == goodyType) {
					val bitBoxState: Boolean = ga.getSpecialBoolean(GoodyNames.BOOLEAN_STATE)
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					novGoody = new BitBox(bgc, goodyID, locVec, rotQuat, scaleVec, bitBoxState)
				}
				else if (GoodyNames.TYPE_BIT_CUBE == goodyType) {
					val bitBoxState: Boolean = ga.getSpecialBoolean(GoodyNames.BOOLEAN_STATE)
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					novGoody = new BitCube(bgc, goodyID, locVec, rotQuat, scaleVec, bitBoxState)
				}
				else if (GoodyNames.TYPE_FLOOR == goodyType) {
					val gcolor: ColorRGBA = ga.getColorOrDefault
					novGoody = new VirtualFloor(bgc, goodyID, locVec, gcolor, true)
				}
				else if (GoodyNames.TYPE_TICTAC_MARK == goodyType) {
					val isAnO: Boolean = ga.getSpecialBoolean(GoodyNames.USE_O)
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					novGoody = new TicTacMark(bgc, goodyID, locVec, rotQuat, scaleVec, isAnO)
				}
				else if (GoodyNames.TYPE_TICTAC_GRID == goodyType) {
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColorOrDefault
					novGoody = new TicTacGrid(bgc, goodyID, locVec, rotQuat, gcolor, scaleVec)
				}
				else if (GoodyNames.TYPE_BOX == goodyType) {
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColorOrDefault
					novGoody = new GoodyBox(bgc, goodyID, locVec, rotQuat, gcolor, scaleVec)
				}
				else if (GoodyNames.TYPE_CROSSHAIR == goodyType) {
					val scaleUniform: java.lang.Float =  getScaleUniformFrom(ga)
					novGoody = new CrossHairGoody(bgc, goodyID, locVec, scaleUniform)
				}
				else if (GoodyNames.TYPE_SCOREBOARD == goodyType) {
					val rowCount: Integer = ga.getSpecialInteger(GoodyNames.ROWS)
					val sizeVec: Array[java.lang.Float] = ga.getSizeVec3D
					val sizeX: Float = sizeVec(0)
					val rowHeight: Float = sizeX
					val scaleUniform: java.lang.Float =  getScaleUniformFrom(ga)
					val textSize: Float = scaleUniform
					myVargler.info4("Scoreboard row count={}, textSize={}, rowHeight={}, locVec={}", rowCount, rowHeight : java.lang.Float, textSize : java.lang.Float, locVec)
					novGoody = new ScoreBoardGoody(bgc, goodyID, locVec, rowHeight, rowCount, textSize)
				}
				else if (GoodyNames.TYPE_TEXT == goodyType) {
					val goodyText: String = ga.getText
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColorOrDefault
					novGoody = new ParagraphGoody(bgc, goodyID, locVec, scaleVec.getX, gcolor, goodyText)
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
