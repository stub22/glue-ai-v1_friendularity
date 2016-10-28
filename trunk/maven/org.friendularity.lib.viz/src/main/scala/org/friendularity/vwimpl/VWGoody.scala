/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.vwimpl

import java.awt.Dimension

import akka.actor.{Actor, ActorContext, ActorRef}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import com.jme3.math.{Quaternion, ColorRGBA, Vector3f}
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.api.thing.WantsThingAction.ConsumpStatus
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.{CameraBinding, VWorldEntityReg, VWorldEntity, GoodyActionExtractor}
import org.cogchar.render.goody.basic.{GoodyBox, VirtualFloor, BasicGoodyCtxImpl, BasicGoodyCtx}
import org.cogchar.render.goody.bit.{TicTacGrid, TicTacMark, BitCube, BitBox}
import org.cogchar.render.goody.flat.{ParagraphGoody, ScoreBoardGoody, CrossHairGoody}
import org.cogchar.render.optic.goody.VWorldCameraEntity
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer.QueueingStyle
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.thact.ThingActExposer
import org.friendularity.vwmsg.{VWRqTAWrapper, VWTARqRdf, VWRqTAWrapImpl, VWorldRequest}

/**
  * Created by Stub22 on 5/25/2016.
  *
  * Handles messages to (sometimes stateful) goody visual objects.
  * One instance of this logic may translate and forward messages for any number
  * of goody objects.
  *	These may involve:
  * create/destroy, attach/detach, movement (smooth or abrupt),
  * goody-state changes, color+transparency changes.
  *
  * Note that currently we don't support any outbound replies or notices from the Goody system.
  *
  * Messages to avatars/bodies and cameras are *not* handled here - they are processed upstream
  * of here in VWThingActRouter.
  *
  * Old BasicGoodyCtx defines these basic capabilities

  * RenderRegistryClient getRRC();
  * VWorldEntityReg getVWER();  -- defines addGoody, removeGoody, getGoody, hasGoodyAt, getAllGoodies
  * Dimension getScreenDimension();
  * void applyNewScreenDimension(Dimension var1);
  * ConsumpStatus consumeAction(ThingActionSpec var1);

  * While old VWorldEntity defines:

  * public Ident getUri()

  * public abstract void setPosition(Vector3f var1, QueueingStyle var2);
  * public void setRotation(Quaternion newRotation, QueueingStyle style)
  * public void setVectorScale(Vector3f scaleVector, QueueingStyle style)
  * public void setUniformScaleFactor(Float scale, QueueingStyle style)

  * public abstract void applyAction(GoodyActionExtractor var1, QueueingStyle var2);
  * public void attachToVirtualWorldNode(Node attachmentNode, QueueingStyle style)
  * public void detachFromVirtualWorldNode(QueueingStyle style)
  * public void applyScreenDimension(Dimension screenDimension)

  */

trait VWGoodyJobLogic extends VarargsLogging {
	protected def getGoodyCtx : BasicGoodyCtx
	protected def processVWGoodyRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwmsg match {

			case taBinWrapMsg: VWRqTAWrapper => processVWGoodyActSpec(taBinWrapMsg, localActorCtx)

			case goodyRdfMsg: VWTARqRdf => processVWGoodyRdfMsg(goodyRdfMsg, slfActr, localActorCtx)
		}
	}
	protected def processVWGoodyActSpec (thingActSpecMsg : VWRqTAWrapper, localActorCtx : ActorContext) : Unit = {
		val actSpec = thingActSpecMsg.getActionSpec
		info4("VWGoodyJobLogic is processing received actSpec of class={}, verb={}, tgtType={} tgtID={}",
					actSpec.getClass, actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		val goodyCtx = getGoodyCtx

		// As of 2016-10-06, this method call routes to the old Cogchar impl in
		//   o.c.render.goody.basic.BasicGoodyCtxImpl.consumeAction(actSpec)
		// However, in the case of CREATE operations, the behavior is overridden in
		// BetterBGC.createByAction.

		goodyCtx.consumeAction(actSpec)
	}

	protected def processVWGoodyRdfMsg (goodyMsg : VWTARqRdf, slfActr : ActorRef, localActorCtx : ActorContext) : Unit = {
		val jenaModel = goodyMsg.asJenaModel(None)
		val exposer = new ThingActExposer {}
		val thingActs : List[ThingActionSpec] = exposer.extractThingActsFromModel(jenaModel)

		if (thingActs.isEmpty) {
			warn1("Found 0 ThingActs in inbound VWGoodyRqRdf message, dumping model:\n {}", jenaModel)
		}
		if (thingActs.length > 1) {
			warn1("Found {} ThingActs in inbound VWGoodyRqRdf, processing in arbitrary order (TODO: sort by timestamp)", thingActs.length : Integer)
		}
		// Process all thingActs, immediately in this thread, in any order.
		for (tas <- thingActs) {

			val specMsg = new VWRqTAWrapImpl(tas)

			processVWGoodyActSpec(specMsg, localActorCtx)
			// We could instead requeue, as we used to do (summer 2016), like so:
			// 	slfActr.tell(specMsg, slfActr)
			// but that could change order of mixed TA processing from client's point of view.
		}

	}
}

class VWGoodyActor(myGoodyCtx : BasicGoodyCtx) extends Actor with VWGoodyJobLogic {
	override protected def getGoodyCtx : BasicGoodyCtx = myGoodyCtx
	def receive = {
		case vwrq: VWorldRequest => {
			processVWGoodyRequest(vwrq, self, context)
		}
	}
}

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
