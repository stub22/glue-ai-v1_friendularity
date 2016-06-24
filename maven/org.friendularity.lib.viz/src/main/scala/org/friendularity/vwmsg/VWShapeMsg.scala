package org.friendularity.vwmsg

import com.jme3.scene.{Geometry, Mesh, Spatial}
import com.jme3.scene.shape.Sphere
import org.appdapter.core.name.Ident
import org.cogchar.render.trial.TextSpatialFactory

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
	def getKnownURI_opt : Option[String] = None
	def expectUniqueURI : Boolean = false

	def getKnownParentID_opt : Option[Ident] = None
	def inFlatSpace : Boolean = false
}

trait VWShapeClearRq extends VWContentRq {
}

case class VWCleaAllShapes()

// These message types are matchable to the fleshier 3D primitives in JMonkey3

trait VWMeshyShapeRq extends VWShapeCreateRq

case class VWSCR_Sphere(myRadius : Float) extends VWMeshyShapeRq

trait VWSCR_Box extends VWMeshyShapeRq

trait VWSCR_Cylinder extends VWMeshyShapeRq

trait VWSCR_Torus extends VWMeshyShapeRq

trait VWSCR_PQTorus extends VWMeshyShapeRq

trait VWSCR_Quad extends VWMeshyShapeRq

case class  VWSCR_TextBox(contentTxt : String) extends VWShapeCreateRq {
	override def inFlatSpace : Boolean = true
}


trait VWSCR_Node extends VWShapeCreateRq

trait VWSCR_CellGrid extends VWShapeCreateRq

