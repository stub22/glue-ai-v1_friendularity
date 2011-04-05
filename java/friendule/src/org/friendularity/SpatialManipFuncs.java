/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity;

import com.jme3.animation.AnimControl;
import com.jme3.animation.Skeleton;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author winston
 */
public class SpatialManipFuncs {
	public static void dumpNodeTree(Node sceneNode, String childPrefix) {
		AnimControl ac = sceneNode.getControl(AnimControl.class);
		if (ac != null) {
			System.out.println (childPrefix + " CONTROL " + ac);
			Skeleton skel = ac.getSkeleton();
			if (skel != null) {
				System.out.println (childPrefix + "    SKELETON " + skel);
			}
		}
		for (Spatial sk : sceneNode.getChildren()) {
			System.out.println(childPrefix + sk);
			if (sk instanceof Node) {
				dumpNodeTree ((Node) sk, "  " + childPrefix);
			}
		}
	}
	public static List<AnimControl> findAnimControls(Node sceneNode) {
		List<AnimControl> results = new ArrayList<AnimControl>();
		AnimControl ac = sceneNode.getControl(AnimControl.class);
		if (ac != null) {
			results.add(ac);
		} else {
			for (Spatial sceneChild : sceneNode.getChildren()) {
				if (sceneChild instanceof Node) {
					List<AnimControl> subRes = findAnimControls((Node) sceneChild);
					results.addAll(subRes);
				}
			}
		}
		return results;
	}
	/*
	 * found sceneKid spatial: body-node (Node)
found sceneKid spatial: rightArm-node (Node)
java.lang.NullPointerException
found sceneKid spatial: leftArm-node (Node)
found sceneKid spatial: head-node (Node)
	 */
/*
 *
 void	setLocalRotation(Matrix3f rotation)
          setLocalRotation sets the local rotation of this node.
 void	setLocalRotation(Quaternion quaternion)
          setLocalRotation sets the local rotation of this node, using a quaterion to build the matrix.
 void	setLocalScale(float localScale)
          setLocalScale sets the local scale of this node.
 void	setLocalScale(float x, float y, float z)
          setLocalScale sets the local scale of this node.
 void	setLocalScale(Vector3f localScale)
          setLocalScale sets the local scale of this node.
 void	setLocalTransform(Transform t)
          setLocalTransform sets the local transform of this spatial.
 void	setLocalTranslation(float x, float y, float z)
          setLocalTranslation sets the local translation of this spatial.
 void	setLocalTranslation(Vector3f localTranslation)
          setLocalTranslation sets the local translation of this spatial.
 *
 * updateGeometricState()
          updateGeometricState updates the lightlist, computes the world transforms, and computes the world bounds for this Spatial.
 *
 protected  void	updateWorldTransforms()
          Should only be called from updateGeometricState().
 Vector3f	worldToLocal(Vector3f in, Vector3f store)
          Convert a vector (in) from world coordinate space to this spatials' local coordinate space.
 */
}
