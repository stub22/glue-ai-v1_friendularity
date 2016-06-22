package org.friendularity.vwmsg

import com.jme3.scene.{Geometry, Mesh, Spatial}
import com.jme3.scene.shape.Sphere

/**
  * Created by Owner on 6/22/2016.
  *
  * See both the "shape" and "debug" packages:
  * https://github.com/jMonkeyEngine/jmonkeyengine/tree/master/jme3-core/src/main/java/com/jme3/scene/shape
  * https://github.com/jMonkeyEngine/jmonkeyengine/tree/master/jme3-core/src/main/java/com/jme3/scene/debug
  *
  *...and this Cogchar comment:
 	* Mesh has these direct subclasses for simpler shapes:
	* AbstractBox, Arrow, Curve, Cylinder, Dome, Grid, Line, ParticleMesh, PQTorus, Quad, SkeletonPoints,
	* SkeletonWire, Sphere, Surface, Torus, WireBox, WireFrustum, WireSphere
  */
trait VWShapeCreateRq extends VWContentRq {
}

// These message types are matchable to the fleshier 3D primitives in JMonkey3

case class VWSCR_Sphere(myRadius : Float) extends VWShapeCreateRq

trait VWSCR_Box extends VWShapeCreateRq

trait VWSCR_Cylinder extends VWShapeCreateRq

trait VWSCR_Torus extends VWShapeCreateRq

trait VWSCR_PQTorus extends VWShapeCreateRq

trait VWSCR_Quad extends VWShapeCreateRq

trait VWSCR_TextBox extends VWShapeCreateRq

trait VWSCR_Node extends VWShapeCreateRq

trait VWShapeMaker {
	def makeForRq(vwscr : VWShapeCreateRq) : Spatial = {
		val mesh : Mesh = vwscr match {
			case sph : VWSCR_Sphere => {
				// zSamp, rSamp, radius
				new Sphere(16, 16, sph.myRadius)
			}
		}
		val geomNameArb : String = "geom_from_msg_shape_" + System.currentTimeMillis()
		val geom = new Geometry(geomNameArb, mesh)
		geom
	}
}
