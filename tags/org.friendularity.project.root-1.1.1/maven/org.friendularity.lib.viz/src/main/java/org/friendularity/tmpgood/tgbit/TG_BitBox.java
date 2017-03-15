/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.tmpgood.tgbit;

import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Torus;
import org.appdapter.core.name.Ident;
import org.cogchar.render.goody.basic.BasicGoodyCtx;

/**
 * A class to implement the Robosteps "BitBox" objects, which as it turns out are not boxes at all
 * See BitCube for a real BitBOX
 * 
 * @author Ryan Biggs <rbiggs@hansonrobokind.com>
 */


public class TG_BitBox extends TG_AbstractBitGoody {
	
	private static final ColorRGBA FALSE_COLOR = ColorRGBA.Blue;
	private static final ColorRGBA TRUE_COLOR = ColorRGBA.Red;
	
	private int zeroIndex;
	private int oneIndex;
	
	public TG_BitBox(BasicGoodyCtx bgc, Ident boxUri, Vector3f initialPosition, Quaternion initialRotation,
					 Vector3f size, boolean boxState) {
		super(bgc, boxUri);
		QueueingStyle qStyle = QueueingStyle.QUEUE_AND_RETURN;
		//myLogger.info("Making a BitBox: size={}, position={}, state={}, URI={}", //TEST ONLY
		//	new Object[]{size, initialPosition, boxState, boxUri.getAbsUriString()}); //TEST ONLY
		if (size == null) {
			getLogger().warn("No size specified for BitBox, defaulting to size = 1");
			size = new Vector3f(1.0f, 1.0f, 1.0f);
		} else {
			if (Math.abs(size.length() - 0.0f) < 0.001f) {
				getLogger().warn("BitBox being created with zero size!");
			}
		} 
		setPositionAndRotation(initialPosition, initialRotation, qStyle);
		setVectorScale(size, qStyle);
		Mesh zeroMesh = new Torus(40,20,1f/5f,5f/6f);
		Mesh oneMesh = new Cylinder(20, 20, 1f/5f, 2f, true);
		zeroIndex = addGeometry(zeroMesh, FALSE_COLOR);
		float[] oneRotationAngles = {(float)(Math.PI/2), 0f, 0f};
		oneIndex = addGeometry(oneMesh, TRUE_COLOR, new Quaternion(oneRotationAngles));
		state = boxState;
	}

	@Override public void attachToVirtualWorldNode(final Node rootNode, QueueingStyle qStyle) {
		attachToVirtualWorldNode(rootNode, state? oneIndex : zeroIndex, qStyle);
	}
	public void attachToVirtualWorldNode(final Node rootNode, boolean boxState, QueueingStyle qStyle) {
		state = boxState;
		attachToVirtualWorldNode(rootNode, qStyle);
	}
	
	@Override public void setState(boolean boxState, QueueingStyle qStyle) {
		int geometryIndex = boxState? oneIndex : zeroIndex;
		setGeometryByIndex(geometryIndex, qStyle);
		state = boxState;
	}
	
}
