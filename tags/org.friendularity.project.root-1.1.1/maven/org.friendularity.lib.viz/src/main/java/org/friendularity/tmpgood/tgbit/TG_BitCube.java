/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

import com.jme3.asset.AssetManager;
import com.jme3.material.Material;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.texture.Texture;
import jme3tools.optimize.TextureAtlas;
import org.appdapter.core.name.Ident;
import org.cogchar.render.goody.basic.BasicGoodyCtx;

import org.cogchar.render.opengl.optic.MatFactory;

/**
 * This class generates BitCubes, another Goody bit object with zeros and ones on opposite sides
 * of a cube which rotates when the state is changed
 * 
 * @author Ryan Biggs <rbiggs@hansonrobokind.com>
 */


public class TG_BitCube extends TG_AbstractBitGoody {
	
	private static final float STATE_TRANSITION_TIME = 2f;
	
	private int geomIndex;
	
	public TG_BitCube(BasicGoodyCtx bgc, Ident boxUri, Vector3f initialPosition, Quaternion initialRotation,
					  Vector3f size, Boolean boxState) {
		super(bgc, boxUri);
		QueueingStyle qStyle = QueueingStyle.QUEUE_AND_RETURN;
		if (size == null) {
			getLogger().warn("No size specified for BitCube, defaulting to size = 1");
			size = new Vector3f(1.0f, 1.0f, 1.0f);
		} else {
			if (Math.abs(size.length() - 0.0f) < 0.001f) {
				getLogger().warn("BitCube being created with zero size!");
			}
		} 
		setPositionAndRotation(initialPosition, initialRotation, qStyle);
		setVectorScale(size, qStyle);
		geomIndex = addCubeGeometry();
		state = boxState;
		setNewGeometryRotationOffset(geomIndex, getStateAdjustedRotationOffset(boxState));
	}
	
	private int addCubeGeometry() {
		Mesh cubeMesh = new TG_BitCubeBox(1f, 1f, 1f);
		TextureAtlas atlas = new TextureAtlas(400, 1200);
		MatFactory matFact = getRenderRegCli().getOpticMaterialFacade(null, null);
		AssetManager assetMgr = matFact.getAssetManager();
		Texture zeroTexture = assetMgr.loadTexture("textures/robosteps/Zero.png");
		Texture oneTexture = assetMgr.loadTexture("textures/robosteps/One.png");
		Texture blankTexture = assetMgr.loadTexture("textures/robosteps/BlankGray.png");
		atlas.addTexture(zeroTexture, "ColorMap");
		atlas.addTexture(oneTexture, "ColorMap");
		atlas.addTexture(blankTexture, "ColorMap");
		Material cubeMaterial = matFact.makeMatWithOptTexture("Common/MatDefs/Misc/Unshaded.j3md", "ColorMap", 
					atlas.getAtlasTexture("ColorMap"));
		return addGeometry(cubeMesh, cubeMaterial, null, new Quaternion());
	}
	
	private Quaternion getStateAdjustedRotationOffset(boolean boxState) {
		float yaw = boxState? (float)Math.PI : 0f;
		return new Quaternion().fromAngles(0f, yaw, 0f);
	}
	
	@Override public void setState(boolean boxState, QueueingStyle qStyle) {
		if (boxState != state) {
			Quaternion initialRotation = getRotation();
			Vector3f position = getPosition();
			Vector3f scale = getScale();
			// A little trick: we use moveViaAnimation to rotate smoothly to the new orientation,
			// then set the new rotation offset and set myRotation back to the original "base" orientation
			moveViaAnimation(position, initialRotation.mult(new Quaternion().fromAngles(0f, (float)Math.PI, 0f)), scale, STATE_TRANSITION_TIME);
			setNewGeometryRotationOffset(geomIndex, getStateAdjustedRotationOffset(boxState));
			setRotation(initialRotation, qStyle);
			state = boxState;
		}
	}
	
}
