package org.friendularity.vw.msg.shp.deep

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.core.name.Ident
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vw.mprt.manip.{MaybeTransform3D, ManipCompletionHandle, ManipDesc, ManipStatusMsg, ManipStatusPropagator, Transform3D}
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Stub22 on 6/22/2016.
  * Mesh logic and comments moved to VWMeshyDesc.scala*
  *
  * These traits defines creation logic for shapes that can be visible mesh+material objects (~= geometries in JME-speak)
  * OR attachment nodes for sub-shapes, cameras, and avatars (~= JmeNode).
  *
  * Clients know only the shapeIDs they have sent, or otherwise mysteriously acquired.
  */

trait KnowsShapeIDsPart {
	def getKnownID_opt : Option[Ident] = None // We allow but don't require a client-assigned shapeID

	def expectEmptySlot : Boolean = false

	def getKnownParentID_opt : Option[Ident] = None // We allow but don't require a parent shapeID.

}

trait ExposedXformParamsPart {
	lazy val EMPTY_XFORM = new MaybeTransform3D {}

	def getInitXform3D_partial : MaybeTransform3D
/*
	def getPosParam_opt : Option[Vector3f] =  getInitXform3D_partial.getPos_opt// getCoreParams3D_opt.map(_.getPos)
	def getRotParam_opt : Option[Quaternion] = getInitXform3D_partial.getRotQuat_opt  // getCoreParams3D_opt.map(_.getRotQuat)
	def getScaleParam_opt : Option[Vector3f] = getInitXform3D_partial.getScl_opt
*/
}

trait ExposedMatParamsPart {
	// def getColorParam_opt : Option[ColorRGBA] = None

	def getMatDescPart : VWMatDesc

}

// The "Exposed" parts are superfluous, yet explanatory of what the "Create" may need to supply.
// The real composition guts for visual primitives is shown in MeshyComposite.
// However for camera and sub-attachment stuff, see traits further down in this file.
// They do not use Exposed API, because when working with non-meshy nodes we expect client to
// do the create and manip steps separately.
trait VWShapeCreateRq extends VWContentRq with KnowsShapeIDsPart
			with ExposedXformParamsPart  {

	def inFlatSpace : Boolean = false // Default is inDeepSpace=3D, override to make flat=2D=true
}


// These message types are matchable to the fleshier 3D primitives in JMonkey3

// case class VWCamWrapShapeCreateRq() extends VWShapeCreateRq

// As discussed above, all these hookup-node shapes have an EMPTY_XFORM by default, because
// we expect clients to send manip rqs separately to explicitly set the xform, after the node-shape is created.

abstract class KnownShapeCreateRqImpl(knownNodeID_opt : Option[Ident], knownParentID_opt : Option[Ident])
			extends VWShapeCreateRq {
	override def getKnownID_opt : Option[Ident] = knownNodeID_opt
	// We generally allow but don't require a parentID.
	override def getKnownParentID_opt : Option[Ident] = knownParentID_opt

	override def getInitXform3D_partial : MaybeTransform3D = EMPTY_XFORM

}
case class VWSCR_KnownShape(knownNodeID_opt : Option[Ident], knownParentID_opt : Option[Ident]) extends
			KnownShapeCreateRqImpl(knownNodeID_opt, knownParentID_opt)


// Can only work as a local message.  Binds an existing JmeNode to a given ID, and optional parent.
// We use this to register CameraNodes after they have been bound to a camera.
case class VWSCR_ExistingNode(existingNode : JmeNode,  nodeID : Ident,
							  knownParentID_opt : Option[Ident])
			extends KnownShapeCreateRqImpl(Option(nodeID), knownParentID_opt)

// Creates a new JmeNode, to be used as a parent for other spatials.
case class VWSCR_Node(knownNodeID : Ident, knownParentID_opt : Option[Ident])
			extends KnownShapeCreateRqImpl(Option(knownNodeID), knownParentID_opt) {

	override def expectEmptySlot : Boolean = true
}

// Create a JmeNode specifically for the purpose of guiding a child cameraNode around, with the
// special ability to re-sync itself to that camera's current position, upon attachment.
case class VWSCR_CamGuideNode(knownNodeID : Ident, knownParentID_opt : Option[Ident])
			extends KnownShapeCreateRqImpl(Option(knownNodeID), knownParentID_opt) {

	override def expectEmptySlot : Boolean = true
}


case class  VWSCR_TextBox(shapeID_opt : Option[Ident], contentTxt : String, myFlatSpace : Boolean,
						  	myXformOfVagueDim : MaybeTransform3D, color : ColorRGBA) extends VWShapeCreateRq {

	override def getKnownID_opt : Option[Ident] = shapeID_opt

	override def inFlatSpace : Boolean = myFlatSpace

	override def getInitXform3D_partial : MaybeTransform3D =  myXformOfVagueDim

}

trait VWSCR_CellGrid extends VWShapeCreateRq {

	override def getInitXform3D_partial : MaybeTransform3D =  EMPTY_XFORM
}
// "AnimControl is a Spatial control that allows manipulation of skeletal animation."
// SpatialTrack(float[] times, Vector3f[] translations, Quaternion[] rotations, Vector3f[] scales)
// Creates a spatial track for the given track data.

/*
AnimationFactory
extends java.lang.Object
A convenience class to easily setup a spatial keyframed animation you can add some keyFrames for a given time or a
given keyFrameIndex, for translation rotation and scale. The animationHelper will then generate an appropriate
 SpatialAnimation by interpolating values between the keyFrames.

Usage is :
- Create the AnimationHelper
- add some keyFrames
- call the buildAnimation() method that will retruna new Animation
- add the generated Animation to any existing AnimationControl

Note that the first keyFrame (index 0) is defaulted with the identy transforms. If you want to change that you have
to replace this keyFrame with any transform you want.

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

Actually what you need is AnimationFactory, it's a convenience class that use a key framed approach
(you set transformation for a spatial at a given time, or keyframe), and allow in the end to generate
 a proper animation for the spatial.

Then the usage is exactly the same than for bone animations via AnimControl and AnimChannel.

Look at TestCinematic for an example, that's how the teapot is animated.

http://code.google.com/p/jmonkeyengine/source/browse/trunk/engine/src/test/jme3test/animation/TestCinematic.java


 */