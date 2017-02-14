package org.friendularity.vw.impl.ta

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{TypedValueMap, ThingActionSpec}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.{PartialTransform3D, MaybeTransform3D}

import java.lang.{Float => JFloat, Integer => JInt, Long => JLong}


/**
  * Created by Owner on 1/26/2017.
  */
trait TARqExtractorHelp {
	import scala.collection.JavaConverters._

	def extractXform_part (taSpec : ThingActionSpec) : MaybeTransform3D = {
		val tvm = taSpec.getParamTVM
		val gax = new GoodyActionExtractor(taSpec)
		extractXform(tvm, gax)
	}
	def extractXform (tvm : TypedValueMap, gax : GoodyActionExtractor) : MaybeTransform3D = {
		val allKeys : Set[Ident] = tvm.iterateKeys().asScala.toSet
		// We assume that presence or absence of a single coordinate indicates that part of
		// each transform: Location, Rotation, Scale.
		// Location
		val loc_opt : Option[Vector3f] = {
			if(allKeys.contains(GoodyNames.LOCATION_X))
				Option(gax.getLocationVec3f)
			else None
		}
		// Rotation
		val rot_opt : Option[Quaternion] = {
			if(allKeys.contains(GoodyNames.ROTATION_MAG_DEG))
				Option(gax.getRotationQuaternion)
			else None
		}
		// Scale
		val scl_opt : Option[Vector3f] = {
			if(allKeys.contains(GoodyNames.SCALE_X))
				Option(gax.getScaleVec3f)
			else {
				if (allKeys.contains(GoodyNames.SCALE_UNIFORM)) {
					val scaleUni : Float = gax.getScaleUniform
					Some(new Vector3f(scaleUni, scaleUni, scaleUni))
				} else None
			}
		}
		// "Partial" here indicates that each of the 3 pieces is optional.
		new PartialTransform3D(loc_opt, rot_opt, scl_opt)
	}
	def extractDuration(tvm : TypedValueMap) : Option[JFloat] = Option(tvm.getAsFloat(GoodyNames.TRAVEL_TIME))

	def extractColor(gax : GoodyActionExtractor) : Option[ColorRGBA] = gax.getColor
}