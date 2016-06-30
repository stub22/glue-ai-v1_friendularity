package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
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
	def getKnownID_opt : Option[Ident] = None // We allow but don't require a client-assigned ID
	// def getKnownURI_opt : Option[String] = None
	def expectEmptySlot : Boolean = false

	def getKnownParentID_opt : Option[Ident] = None // We allow but don't require a parentID.

	def inFlatSpace : Boolean = false // Default is inDeepSpace=3D, override to make flat=2D=true
}

trait Located3D {
	def getPos : Vector3f
}
trait Rotated3D {
	def getRotQuat : Quaternion
}
trait Scaled3D {
	def getScale : Vector3f
}
trait BasicSpatialParams3D extends Located3D with Rotated3D with Scaled3D


trait Colored {
	def getColor : ColorRGBA
}

trait CoreParams3D extends BasicSpatialParams3D with Colored

class OrdinaryParams3D(myPos3f : Vector3f, myRotQuat : Quaternion, myScale3f : Vector3f, myColor : ColorRGBA) extends CoreParams3D {
	override def getPos : Vector3f = myPos3f
	override def getRotQuat : Quaternion = myRotQuat
	override def getScale : Vector3f = myScale3f
	override def getColor : ColorRGBA = myColor
}
object ParamsConstants {
	private val  zeroPos = Vector3f.ZERO

	val dullishColor = new ColorRGBA(1.0f, 0.5f, 0.2f, 0.5f);
	val dullestParams = new OrdinaryParams3D(Vector3f.ZERO, Quaternion.IDENTITY, Vector3f.UNIT_XYZ, dullishColor)
}

trait VWShapeClearRq extends VWContentRq {

}

case class VWClearAllShapes() extends VWShapeClearRq

// These message types are matchable to the fleshier 3D primitives in JMonkey3

trait VWMeshyShapeRq extends VWShapeCreateRq {
	def getCoreParams3D : Option[CoreParams3D] = None
	def getColorParam : Option[ColorRGBA] = getCoreParams3D.map(_.getColor)
	def getPosParam : Option[Vector3f] = getCoreParams3D.map(_.getPos)
	def getRotParam : Option[Quaternion] = getCoreParams3D.map(_.getRotQuat)
}

case class VWSCR_Sphere(myRadius : Float, myCoreParams : CoreParams3D) extends VWMeshyShapeRq {
	override def getCoreParams3D : Option[CoreParams3D] = Option(myCoreParams)
}

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
// "AnimControl is a Spatial control that allows manipulation of skeletal animation."
// SpatialTrack(float[] times, Vector3f[] translations, Quaternion[] rotations, Vector3f[] scales)
// Creates a spatial track for the given track data.

/*
AnimationFactory
extends java.lang.Object
A convenience class to easily setup a spatial keyframed animation you can add some keyFrames for a given time or a given keyFrameIndex, for translation rotation and scale. The animationHelper will then generate an appropriate SpatialAnimation by interpolating values between the keyFrames.

Usage is :
- Create the AnimationHelper
- add some keyFrames
- call the buildAnimation() method that will retruna new Animation
- add the generated Animation to any existing AnimationControl

Note that the first keyFrame (index 0) is defaulted with the identy transforms. If you want to change that you have to replace this keyFrame with any transform you want.

public Animation buildAnimation()

Creates an Animation based on the keyFrames previously added to the helper.

 */

// Jme3 Spatial
// addControl(Control control)
// All Known Implementing Classes:
// AbstractControl, AnimControl, BillboardControl, CameraControl, CharacterControl, ChaseCamera,
// EffectTrack.KillParticleControl, GhostControl, KinematicRagdollControl, LightControl, LodControl,
// MotionEvent, MotionTrack, MultiTerrainLodControl, NormalRecalcControl,
// ParticleEmitter.ParticleEmitterControl, RigidBodyControl, SkeletonControl, StatsView,
// TerrainGridLodControl, TerrainLodControl, UpdateControl, VehicleControl


/*
https://hub.jmonkeyengine.org/t/animations-in-jme/17554/12

Sorry it's an error in the javadoc.

Actually what you need is AnimationFactory, it's a convenience class that use a key framed approach (you set transformation for a spatial at a given time, or keyframe), and allow in the end to generate a proper animation for the spatial.

Then the usage is exactly the same than for bone animations via AnimControl and AnimChannel.

Look at TestCinematic for an example, that's how the teapot is animated.

http://code.google.com/p/jmonkeyengine/source/browse/trunk/engine/src/test/jme3test/animation/TestCinematic.java


 */