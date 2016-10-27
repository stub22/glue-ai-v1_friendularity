package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.{Geometry, Mesh, Spatial}
import com.jme3.scene.shape.Sphere
import com.jme3.scene.{Node => JmeNode}

import org.appdapter.core.name.Ident
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.cpmsg.CPStrongTeller

/**
  * Created by Stub22 on 6/22/2016.
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

	def expectEmptySlot : Boolean = false

	def getKnownParentID_opt : Option[Ident] = None // We allow but don't require a parentID.

	def inFlatSpace : Boolean = false // Default is inDeepSpace=3D, override to make flat=2D=true
}

case class VWShapeAttachRq(knownID : Ident, knownParentID_opt : Option[Ident]) extends VWContentRq


trait VWShapeManipRq extends VWContentRq {
	def getTgtShapeID : Ident
	def getManipDesc : ManipDesc
	def getStatusHandler_opt : Option[ManipCompletionHandle] = None
}
case class ShapeManipRqImpl(myTgtShapeID : Ident, myManipDesc : ManipDesc,
							statusTlr_opt : Option[CPStrongTeller[ManipStatusMsg]]) extends VWShapeManipRq {
	override def getTgtShapeID: Ident = myTgtShapeID

	override def getManipDesc: ManipDesc = myManipDesc

	lazy val statusHandlerOpt = statusTlr_opt.map(tlr => new ManipStatusPropagator(Option(tlr)))
	override def getStatusHandler_opt : Option[ManipCompletionHandle] = statusHandlerOpt
}

// Actual msgs we expect, with case-class impls below.
trait DoTransformAbsoluteNow extends VWShapeManipRq {
	// Abruptly moves the target to this new state.
	// "Abs" pos is w.r.t. parent, not world.
	def getAbsXform : Transform3D
}
trait DoTransformIncrementNow extends VWShapeManipRq {
	// Incremental change in each xform, based on current state of the target.
	// Use cases are mainly for casual scripting and experiments.
	// Is still a jump-state, presumably just a relatively small one.
	// Not intended for smooth animation - see Smoove* classes for that feature.
	// (Or for cine-style waypoint anim, see the VWCinePath)
	def getIncrementXform : Transform3D
}



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

trait VWShapeClearRq extends VWContentRq

case class VWClearAllShapes() extends VWShapeClearRq

case class VWShapeDetachRq(shapeID : Ident) extends VWContentRq


// These message types are matchable to the fleshier 3D primitives in JMonkey3

// case class VWCamWrapShapeCreateRq() extends VWShapeCreateRq

class KnownShapeCreateRqImpl(knownNodeID_opt : Option[Ident], knownParentID_opt : Option[Ident]) extends VWShapeCreateRq {
	override def getKnownID_opt : Option[Ident] = knownNodeID_opt
	// We generally allow but don't require a parentID.
	override def getKnownParentID_opt : Option[Ident] = knownParentID_opt
}

// Can only work as a local message.  Binds an existing JmeNode to a given ID, and optional parent.
case class VWSCR_ExistingNode(existingNode : JmeNode,  nodeID : Ident,
							  knownParentID_opt : Option[Ident])
			extends KnownShapeCreateRqImpl(Option(nodeID), knownParentID_opt)

// Creates a new JmeNode.
case class VWSCR_Node(knownNodeID : Ident, knownParentID_opt : Option[Ident])
			extends KnownShapeCreateRqImpl(Option(knownNodeID), knownParentID_opt) {
//	override def getKnownID_opt : Option[Ident] = Option(knownNodeID)
	override def expectEmptySlot : Boolean = true
}

trait VWMeshyShapeRq extends VWShapeCreateRq {
	def getCoreParams3D : Option[CoreParams3D] = None
	def getColorParam : Option[ColorRGBA] = getCoreParams3D.map(_.getColor)
	def getPosParam : Option[Vector3f] = getCoreParams3D.map(_.getPos)
	def getRotParam : Option[Quaternion] = getCoreParams3D.map(_.getRotQuat)
}

trait TwoPartMeshyShapeRq extends VWShapeCreateRq with VWMeshyShapeRq  {
	def getKnownIdentsPart : VWShapeCreateRq
	def getMeshyDescPart : VWMeshyShapeRq

	override def getKnownID_opt  = getKnownIdentsPart.getKnownID_opt
	override def getKnownParentID_opt  = getKnownIdentsPart.getKnownParentID_opt

	override def getCoreParams3D = getMeshyDescPart.getCoreParams3D
	override def getColorParam = getMeshyDescPart.getColorParam
	override def getPosParam  = getMeshyDescPart.getPosParam
	override def getRotParam  = getMeshyDescPart.getRotParam

}
case class VWSCR_MeshyCmpnd(idsPart : VWShapeCreateRq, meshyPart : VWMeshyShapeRq) extends TwoPartMeshyShapeRq {
	override def getKnownIdentsPart: VWShapeCreateRq = idsPart

	override def getMeshyDescPart: VWMeshyShapeRq = meshyPart
}

case class VWSCR_Sphere(myRadius : Float, myCoreParams : CoreParams3D) extends VWMeshyShapeRq {

	override def getCoreParams3D : Option[CoreParams3D] = Option(myCoreParams)

}

// Each of these is the inner-meshy part of a message to create one of the JME3 primitive
// shape kinds.

case class VWSCR_Box() extends VWMeshyShapeRq

case class VWSCR_Cylinder() extends VWMeshyShapeRq

case class VWSCR_Torus() extends VWMeshyShapeRq

case class VWSCR_PQTorus() extends VWMeshyShapeRq

case class VWSCR_Quad() extends VWMeshyShapeRq

case class  VWSCR_TextBox(contentTxt : String) extends VWShapeCreateRq {
	override def inFlatSpace : Boolean = true
}


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