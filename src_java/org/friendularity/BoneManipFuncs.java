/*
 *  Copyright 2011 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class BoneManipFuncs {
/*
 *
 * Bone describes a bone in the bone-weight skeletal animation system. A bone contains a name and an index, as well as relevant transformation data.
 * 
 *  void	addChild(Bone bone)
          Add a new child to this bone.
 Node	getAttachmentsNode()
          Returns the attachment node.
 java.util.ArrayList<Bone>	getChildren()

 Vector3f	getInitialPos()

 Quaternion	getInitialRot()

 Vector3f	getLocalPosition()

 Quaternion	getLocalRotation()

 Vector3f	getLocalScale()

 Vector3f	getModelSpacePosition()

 Quaternion	getModelSpaceRotation()

 Vector3f	getModelSpaceScale()

 java.lang.String	getName()

 Bone	getParent()

 Vector3f	getWorldBindInversePosition()

 Quaternion	getWorldBindInverseRotation()

 Vector3f	getWorldBindInverseScale()


 void	setAttachmentsNode(Node attachNode)
          Used internally after model cloning.
 void	setBindTransforms(Vector3f translation, Quaternion rotation)

 void	setBindTransforms(Vector3f translation, Quaternion rotation, Vector3f scale)
          Sets local bind transform for bone.
 void	setUserControl(boolean enable)
          If enabled, user can control bone transform with setUserTransforms.
 void	setUserTransforms(Vector3f translation, Quaternion rotation, Vector3f scale)
          Set user transform.
 void	setUserTransformsWorld(Vector3f translation, Quaternion rotation)
          Must update all bones in skeleton for this to work.
 *
 *
 **************** Skeleton **********************
 * A skeleton is a hierarchy of bones. Skeleton updates the world transforms to reflect the current local animated matrixes.
 *
 *  Matrix4f[]	computeSkinningMatrices()

 Bone	getBone(int index)

 Bone	getBone(java.lang.String name)

 int	getBoneCount()

 int	getBoneIndex(Bone bone)

 int	getBoneIndex(java.lang.String name)

 Bone[]	getRoots()

 void	read(JmeImporter im)

 void	reset()
          Reset the skeleton to bind pose.
 void	resetAndUpdate()

 void	setBindingPose()
          Saves the current skeleton state as it's binding pose.
 java.lang.String	toString()

 void	updateWorldVectors()
          Updates world transforms for all bones in this skeleton.
 void	write(JmeExporter ex)
 */
}
