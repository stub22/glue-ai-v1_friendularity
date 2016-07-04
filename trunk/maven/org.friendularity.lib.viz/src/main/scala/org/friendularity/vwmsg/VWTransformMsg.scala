package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f}

/**
  * Created by Owner on 7/2/2016.
  */

// These types describe transforms, which are used in composing bigger messages.
// They are not intended to be sent directly to actors.
// They are marked with "3D" (and when we get to it, "2D").

trait Located3D {
	def getPos : Vector3f
}
trait Rotated3D {
	def getRotQuat : Quaternion
}
trait Scaled3D {
	def getScale : Vector3f
}
trait Transform3D extends Located3D with Rotated3D with Scaled3D

// Different impls of Transform3D
// Fancier:  Wrap subunits, which can be easier if computation is nontrivial
class CompoundXform3D(loc: Located3D, rot: Rotated3D, scale: Scaled3D) extends Transform3D {
	override def getPos : Vector3f = loc.getPos
	override def getRotQuat: Quaternion = rot.getRotQuat
	override def getScale: Vector3f = scale.getScale
}
// ...OR just supply some nice known scalar values.
class TransformParams3D(myPos3f : Vector3f, myRotQuat : Quaternion, myScale3f : Vector3f) extends Transform3D {
	override def getPos : Vector3f = myPos3f
	override def getRotQuat : Quaternion = myRotQuat
	override def getScale : Vector3f = myScale3f
}
// ... OR, TODO:  supply only optional components, with nice defaults filled in automagically

trait Pointed3D { // Used for Cameras, presumes there is a well defined meaning of "pointing" the thing
	def getPointDir : Vector3f
}
trait CamState3D extends Located3D with Pointed3D

case class CamStateParams3D(myWorldPos : Vector3f, myPointDir : Vector3f) extends CamState3D {

	override def getPos: Vector3f = myWorldPos

	override def getPointDir: Vector3f = myPointDir
}

// More advanced topic, not urgent:  Demarking between "no value" and "default value", or "leave unchanged" vs "reset"


