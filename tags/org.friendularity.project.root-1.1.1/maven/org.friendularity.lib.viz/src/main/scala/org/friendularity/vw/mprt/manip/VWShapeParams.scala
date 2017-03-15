package org.friendularity.vw.mprt.manip

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}

/**
  * File created with code moved here from VWShapeMsg.scala by Stub22 on 1/17/2017.
  */
trait Colored {
	def getColor : ColorRGBA
}

trait CoreParams3D extends Transform3D with Colored

class OrdinaryParams3D(pos3f : Vector3f, rotQuat : Quaternion, scale3f : Vector3f, myColor : ColorRGBA)
			extends TransformParams3D(pos3f, rotQuat, scale3f) with CoreParams3D {
	override def getColor : ColorRGBA = myColor
}
object ParamsConstants {
	private val  zeroPos = Vector3f.ZERO

	val dullishColor = new ColorRGBA(1.0f, 0.5f, 0.2f, 0.5f);
	val dullestParams = new OrdinaryParams3D(Vector3f.ZERO, Quaternion.IDENTITY, Vector3f.UNIT_XYZ, dullishColor)
}

