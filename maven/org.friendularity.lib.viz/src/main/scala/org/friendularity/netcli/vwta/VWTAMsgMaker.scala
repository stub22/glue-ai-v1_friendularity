package org.friendularity.netcli.vwta

import com.jme3.math.Vector3f
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, SerTypedValueMap}
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.friendularity.vwimpl.IdentHlp
import org.friendularity.vwmsg.{MaybeTransform3D, MaybeScaled3D, MaybeRotated3D, MaybeLocated3D}

/**
  * Created by Stub22 on 9/5/2016.
  */
trait VWTAMsgMaker extends IdentHlp {
	val myAgentID : Ident = makeStampyRandyIdentAnon()

	def makeTASpec(entityID : Ident, typeID : Ident, verbID : Ident, paramSerMap: SerTypedValueMap) : ThingActionSpec = {
		val actRecID : Ident = makeStampyRandyIdent("actSpec")
		// gar.writeToMap(paramWriter)
		val srcAgentID: Ident = myAgentID
		val postedTStampMsec: Long = System.currentTimeMillis
		val actionSpec = new BasicThingActionSpec(actRecID, entityID, typeID, verbID, srcAgentID, paramSerMap, postedTStampMsec)
		actionSpec
	}
	def writePos(gapw : GoodyActionParamWriter, maybeLocated3D: MaybeLocated3D) : Unit = {
		if (maybeLocated3D.getPos_opt.isDefined) {
			val pos = maybeLocated3D.getPos
			gapw.putLocation(pos.getX, pos.getY, pos.getZ)
		}
	}
	def writeRot(gapw : GoodyActionParamWriter, maybeRot: MaybeRotated3D) : Unit = {
		val actualRot_opt =
			if (maybeRot.getRotQuat_opt.isDefined) {
				val rotQuat = maybeRot.getRotQuat
				val rotAxis = new Vector3f()
				val rotAngleRad : Float = rotQuat.toAngleAxis(rotAxis)
				val rotAngleDeg : Float = Math.toDegrees(rotAngleRad.asInstanceOf[Double]).asInstanceOf[Float];
				gapw.putRotation(rotAxis.getX, rotAxis.getY, rotAxis.getZ, rotAngleDeg)
			}
	}
	def writeScale(gapw : GoodyActionParamWriter, maybeScale: MaybeScaled3D) : Unit = {
		if (maybeScale.getScl_opt.isDefined) {
			val scl = maybeScale.getScale
			gapw.putScaleVec(scl.getX, scl.getY, scl.getZ)
		}
	}
	def writeXform3D(gapw : GoodyActionParamWriter, mayXform : MaybeTransform3D) : Unit = {
		writePos(gapw, mayXform)
		writeRot(gapw, mayXform)
		writeScale(gapw, mayXform)
	}
	def writeXform3D(paramSerMap: SerTypedValueMap, mayXform : MaybeTransform3D) : Unit = {
		val gapw = new GoodyActionParamWriter(paramSerMap)
		writeXform3D(gapw, mayXform)
	}
}