package org.friendularity.vwgoody

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.{VWorldEntity, GoodyActionExtractor}
import org.cogchar.render.goody.basic.{GoodyBox, VirtualFloor, BasicGoodyCtx, BasicGoodyCtxImpl}
import org.cogchar.render.goody.bit.{TicTacGrid, TicTacMark, BitCube, BitBox}
import org.cogchar.render.goody.flat.{ParagraphGoody, ScoreBoardGoody, CrossHairGoody}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor

/**
  * This code moved to new file on 12/14/2016.
  */

class BetterBGC(rrc : RenderRegistryClient, winStatMon : WindowStatusMonitor)
			extends BasicGoodyCtxImpl(rrc, winStatMon) {
	val myVargler = new VarargsLogging {}

	def getScaleVectorFrom(goodyActionExtractor: GoodyActionExtractor): Vector3f = {
		var scaleVec: Vector3f = goodyActionExtractor.getScaleVec3f
		val scaleUniform: java.lang.Float = goodyActionExtractor.getScaleUniform
		if ((scaleVec == null) && (scaleUniform != null)) {
			scaleVec = new Vector3f(scaleUniform, scaleUniform, scaleUniform)
		}
		scaleVec
	}

	def getScaleUniformFrom(goodyActionExtractor: GoodyActionExtractor): java.lang.Float = {
		var scaleUniform: java.lang.Float = goodyActionExtractor.getScaleUniform
		val scaleVec: Vector3f = goodyActionExtractor.getScaleVec3f

		if ((scaleUniform == null) && (scaleVec != null)) {
			if (java.lang.Float.compare(scaleVec.getX, scaleVec.getY) == 0 && java.lang.Float.compare(scaleVec.getX, scaleVec.getZ) == 0) {
				scaleUniform = scaleVec.getX
			} else {
				throw new IllegalStateException("Could not find uniform scale in GoodyActionExtractor.")
			}
		}
		scaleUniform
	}

	/**
	  * Changed this method to unpack variables only if they are used by the current
	  *  {@link VWorldEntity}. If we don't we get nullPointerExceptions when trying to apply a type
	  * to null values.
	  * - Ben[2016-09-29]
	  */
	override protected def createByAction(ga: GoodyActionExtractor): VWorldEntity = {
		var novGoody: VWorldEntity = null

		if (ga.getKind eq GoodyActionExtractor.Kind.CREATE) {
			try {
				val goodyID: Ident = ga.getGoodyID
				val goodyType: Ident = ga.getType
				val locVec: Vector3f = ga.getLocationVec3f

				val bgc: BasicGoodyCtx = this
				myVargler.info1("BetterBGC seeking match for goodyType={}", goodyType)
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
					val gcolor: ColorRGBA = ga.getColor
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
					val gcolor: ColorRGBA = ga.getColor
					novGoody = new TicTacGrid(bgc, goodyID, locVec, rotQuat, gcolor, scaleVec)
				}
				else if (GoodyNames.TYPE_BOX == goodyType) {
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColor
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
					getLogger.info("Scoreboard row count=" + rowCount + ", rowHeight=" + rowHeight + ", textSize=" + textSize + ", locVec=" + locVec)
					novGoody = new ScoreBoardGoody(bgc, goodyID, locVec, rowHeight, rowCount, textSize)
				}
				else if (GoodyNames.TYPE_TEXT == goodyType) {
					val goodyText: String = ga.getText
					val scaleVec: Vector3f = getScaleVectorFrom(ga)
					val gcolor: ColorRGBA = ga.getColor
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
/*  Here is what the inherited consumeAction method looks like as of Summer 2016.

	override def consumeAction(actionSpec: ThingActionSpec): ConsumpStatus = {
		getLogger.info("The targetThingType is {}", actionSpec.getTargetThingTypeID)
		val ga: GoodyActionExtractor = new GoodyActionExtractor(actionSpec)
		val gid: Ident = ga.getGoodyID
		val vwer = getVWER
		var goodyOne: VWorldEntity = vwer.getGoody(gid)
		val kind: GoodyActionExtractor.Kind = ga.getKind
		getLogger.info("The kind of Goody inspected is {}", kind)

		if (kind != null) {
			ga.getKind match {
				case GoodyActionExtractor.Kind.CREATE => {
					if (vwer.hasGoodyAt(gid)) {
						getLogger.warn("Goody already created! Ignoring additional creation request for goody: {}", gid)
					}
					else {
						goodyOne = createAndAttachByAction(ga, QueueingStyle.QUEUE_AND_RETURN)
						if (goodyOne != null) {
							vwer.addGoody(goodyOne)
							return ConsumpStatus.USED
						}
					}

				}
				case GoodyActionExtractor.Kind.DELETE => {
					if (!vwer.hasGoodyAt(gid)) {
						myVargler.warn1("Could not delete goody because it does not exist: {}", gid)
					}
					else {
						vwer.removeGoody(goodyOne)
						return ConsumpStatus.USED
					}

				}
				case _ => {
					try {
						goodyOne.applyAction(ga, QueueingStyle.QUEUE_AND_RETURN)
						return ConsumpStatus.USED
					}
					catch {
						case e: Exception => {
							myVargler.warn2("Problem attempting to update goody with URI: {}", gid, e)
						}
					}
				}
			}
		}
		return ConsumpStatus.IGNORED
	}
*/
