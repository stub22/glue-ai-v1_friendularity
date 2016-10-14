package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f}
import org.cogchar.api.vworld.GoodyActionParamWriter

/**
  * Created by Owner on 7/2/2016.
  */

// These types describe transforms, which are used in composing bigger messages.
// They are not intended to be sent directly to actors.
// They are marked with "3D" (and when we get to it, "2D").

trait MaybeLocated3D {
	def getPos_opt : Option[Vector3f] = None
	def getDefaultPos : Vector3f = Vector3f.ZERO
	def getPos : Vector3f = getPos_opt.getOrElse(getDefaultPos)

}
trait Located3D extends MaybeLocated3D {
	def getPosX : Float = getPos.getX
	def getPosY : Float = getPos.getY
	def getPosZ : Float = getPos.getZ
}
trait MaybeRotated3D {
	def getRotQuat_opt : Option[Quaternion] = None
	def getDefaultRot : Quaternion = Quaternion.IDENTITY
	def getRotQuat : Quaternion = getRotQuat_opt.getOrElse(getDefaultRot)
}
trait Rotated3D extends MaybeRotated3D {

}
trait MaybeScaled3D {
	def getScl_opt : Option[Vector3f] = None
	def getDefaultScl : Vector3f = Vector3f.UNIT_XYZ
	def getScale : Vector3f = getScl_opt.getOrElse(getDefaultScl)
}
trait Scaled3D extends MaybeScaled3D {
	def getScaleX : Float = getScale.getX
	def getScaleY : Float = getScale.getY
	def getScaleZ : Float = getScale.getZ
}

// User must check for existence of particular parts, and apply defaults as desired.
trait MaybeTransform3D extends MaybeLocated3D with MaybeRotated3D with MaybeScaled3D {

}

case class PartialTransform3D(posOpt : Option[Vector3f], rotOpt : Option[Quaternion], sclOpt : Option[Vector3f]) extends MaybeTransform3D {
	override def getPos_opt : Option[Vector3f] = posOpt
	override def getRotQuat_opt : Option[Quaternion] = rotOpt
	override def getScl_opt : Option[Vector3f] = sclOpt
}

// User may assume that ambiguity of "missing" parts is resolved (in local/current/global best way)
trait Transform3D extends MaybeTransform3D with Located3D with Rotated3D with Scaled3D


// Different impls of Transform3D
// Fancier:  Wrap subunits, which can be easier if computation is nontrivial
case class CompoundXform3D(loc: Located3D, rot: Rotated3D, scale: Scaled3D) extends Transform3D {
	override def getPos : Vector3f = loc.getPos
	override def getRotQuat: Quaternion = rot.getRotQuat
	override def getScale: Vector3f = scale.getScale
	override def getPos_opt : Option[Vector3f] = loc.getPos_opt
	override def getRotQuat_opt : Option[Quaternion] = rot.getRotQuat_opt
	override def getScl_opt : Option[Vector3f] = scale.getScl_opt

}
// ...OR just supply some nice known scalar values.  (Note that "nulls" supplied here are mapped to None).
case class TransformParams3D(myPos3f : Vector3f, myRotQuat : Quaternion, myScale3f : Vector3f) extends Transform3D {
	override def getPos : Vector3f = myPos3f
	override def getRotQuat : Quaternion = myRotQuat
	override def getScale : Vector3f = myScale3f
	override def getPos_opt : Option[Vector3f] = Option(myPos3f)
	override def getRotQuat_opt : Option[Quaternion] = Option(myRotQuat)
	override def getScl_opt : Option[Vector3f] = Option(myScale3f)
}
// ... OR, supply only optional components, with nice defaults filled in automagically.
// However note that the meaning of the transform fields can be interpreted as absolute or relative.
trait MakesTransform3D {
	def makeRelativeXForm(pos_opt : Option[Vector3f], rot_opt : Option[Quaternion], scale_opt : Option[Vector3f])
					: Transform3D = {
		val pos : Vector3f = pos_opt.getOrElse(Vector3f.ZERO)
		val rot : Quaternion = rot_opt.getOrElse(Quaternion.IDENTITY)
		val scl : Vector3f = scale_opt.getOrElse(Vector3f.UNIT_XYZ)
		new TransformParams3D(pos, rot, scl)
	}
	def makeDefiniteXForm(src : MaybeTransform3D): Transform3D = {
		if (src.isInstanceOf[Transform3D]) src.asInstanceOf[Transform3D]
		else new TransformParams3D(src.getPos, src.getRotQuat, src.getScale)
	}
}
trait Pointed3D { // Used for Cameras, presumes there is a well defined meaning of "pointing" the thing
	def getPointDir : Vector3f
}
trait CamState3D extends Located3D with Pointed3D

case class CamStateParams3D(myWorldPos : Vector3f, myPointDir : Vector3f) extends CamState3D {

	override def getPos: Vector3f = myWorldPos

	override def getPointDir: Vector3f = myPointDir
}

// More advanced topic, not urgent:  Demarking between "no value" and "default value", or "leave unchanged" vs "reset"

