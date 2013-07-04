/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.api.west;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;

import com.jme3.bullet.PhysicsSpace;
import com.jme3.bullet.collision.shapes.SphereCollisionShape;
import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.bullet.util.CollisionShapeFactory;
import com.jme3.collision.CollisionResults;
import com.jme3.font.BitmapText;
import com.jme3.input.InputManager;
import com.jme3.material.Material;
import com.jme3.material.RenderState.BlendMode;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Ray;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Sphere;

import org.appdapter.core.name.Ident;
import org.cogchar.render.goody.basic.DataballGoodyBuilder;
import org.cogchar.render.opengl.optic.CameraMgr;
import org.cogchar.render.opengl.optic.MatFactory;
import org.cogchar.render.opengl.scene.DeepSceneMgr;
import org.cogchar.render.opengl.scene.FlatOverlayMgr;
import org.cogchar.render.opengl.scene.GeomFactory;
import org.cogchar.render.opengl.scene.TextMgr;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.slf4j.Logger;
/**
 * @author Stu B. <www.texpedient.com>
 */

public class ShapeAnimator {
	private		Map<Ident, VizShape> myShapesByIdent = new HashMap<Ident, VizShape>();
	
	// private		RenderRegistryClient	myRRC;
	private		Material myStandardMaterial;
	private		Node mySubsysNode = new Node("shape_animator_25");
	
	public void setupMaterials(RenderRegistryClient	rrc) { 
		MatFactory matFactory = rrc.getOpticMaterialFacade(null, null);
		myStandardMaterial = matFactory.makeMatWithOptTexture("Common/MatDefs/Light/Lighting.j3md", "SpecularMap", null);
	}
	
	public void enable_onRendThrd(RenderRegistryClient	rrc) {
		if (mySubsysNode != null) {
			DeepSceneMgr dsm = rrc.getSceneDeepFacade(null);
			dsm.attachTopSpatial(mySubsysNode);
		}
	}
	public void disable_onRendThrd(RenderRegistryClient	rrc) {
		if (mySubsysNode != null) {
			DeepSceneMgr dsm = rrc.getSceneDeepFacade(null);
			dsm.detachTopSpatial(mySubsysNode);
		}		
	}
	public void attachChild_onRendThrd(RenderRegistryClient	rrc, VizShape child) {
		myShapesByIdent.put(child.myIdent, child);
		if (child.myGeom == null) {
			child.setupGeom(this, rrc);
		}
		mySubsysNode.attachChild(child.myGeom);
	}

	static class VizShape {
		Ident				myIdent;
		Vector3f			myPosVec;
		float				myRadius;
		// Strangely it seems RGBA is the only colorspace directly supported by JME3 core API - true? (No HSV, YUV)
		ColorRGBA			myColor;
		
		private	Geometry			myGeom;
		// RigidBodyControl	myRigidBodyControl;
		private	Material			myMaterial;

		
		VizShape(Ident id, Vector3f initPos, float initRadius, ColorRGBA initColor) {
			myIdent = id;
			myPosVec = initPos;
			myRadius = initRadius;
			myColor = initColor;
		} 
		public void setupGeom(ShapeAnimator sa, RenderRegistryClient rrc) { 
			// Copied+modified from DataballGoodyBuilder
			Sphere sphereMesh = new Sphere(20, 20, myRadius);
			myMaterial = sa.myStandardMaterial.clone();
			myMaterial.setBoolean("UseMaterialColors", true);
			myMaterial.setColor("Diffuse", myColor);
			myMaterial.setColor("Ambient", myColor);
			myMaterial.setColor("Specular", myColor);
			myMaterial.setFloat("Shininess", 25f);
			// 		material.getAdditionalRenderState().setBlendMode(BlendMode.Alpha);
			//  geometry.setQueueBucket(Bucket.Transparent);
			RigidBodyControl optRBC = null;
			// myRigidBodyControl = new RigidBodyControl(sphereShape(size), (float) (pow(size, 3) * MASS_COEFFICIENT));
			// control.setRestitution(0.5f);
			String emptySelector = null;
			GeomFactory geomFactory = rrc.getSceneGeometryFacade(emptySelector);
			
			myGeom = geomFactory.makeGeom(myIdent.getLocalName(), sphereMesh, myMaterial, optRBC);
		}
 

	}
	/*
			myRenderContext.enqueueCallable(new Callable<Void>() { // Do this on main render thread

				@Override
				public Void call() throws Exception {
					//geometry.addControl(control);
					myPhysics.add(control);
					myBallsNode.attachChild(geometry);
					control.setPhysicsLocation(initialPosition); // Probably unnecessary - setting this here, in reset() above, and using resetAllBalls in buildModelFromTurtle because they don't want to go to the initial position! Probably some sort of jME concurrency thing...
					return null;
				}
			});
		}
		*/
}
