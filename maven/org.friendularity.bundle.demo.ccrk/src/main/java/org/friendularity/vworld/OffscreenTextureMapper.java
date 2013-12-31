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

package org.friendularity.vworld;
import com.jme3.app.SimpleApplication;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import com.jme3.renderer.ViewPort;
import com.jme3.scene.Geometry;
import com.jme3.scene.shape.Box;
import com.jme3.texture.FrameBuffer;
import com.jme3.texture.Image.Format;
import com.jme3.texture.Texture;
import com.jme3.texture.Texture2D;

import com.jme3.scene.Node;

import com.jme3.renderer.RenderManager;
import com.jme3.asset.AssetManager;
import com.jme3.texture.Image;
import com.jme3.texture.plugins.AWTLoader;
import java.awt.image.BufferedImage;
import org.cogchar.render.opengl.optic.MatFactory;
import org.cogchar.render.opengl.optic.TextureFactory;
import org.cogchar.render.opengl.optic.ViewportFacade;
import org.cogchar.render.sys.registry.RenderRegistryClient;

import org.cogchar.render.opengl.scene.DeepSceneMgr;

import org.appdapter.core.log.BasicDebugger;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class OffscreenTextureMapper {
	private ViewPort myOffscrenViewport;
	private Material myOffscreenBoxMaterial = new Material();
	private Geometry myOffscreenBoxGeom;
	
	private float myOffscreenBoxRotationAngle = 0;
	
	// This creates a texture written to from a JME3Camera
	public Texture setupOffscreenView_onRendThrd(RenderManager renderMgr, AssetManager assetMgr, float tpf) {
		Camera offCamera = new Camera(512, 512);

		myOffscrenViewport = renderMgr.createPreView("Offscreen View", offCamera);
		myOffscrenViewport.setClearFlags(true, true, true);
		myOffscrenViewport.setBackgroundColor(ColorRGBA.DarkGray);

		// create offscreen framebuffer
		FrameBuffer offBuffer = new FrameBuffer(512, 512, 1);

		//setup framebuffer's cam
		offCamera.setFrustumPerspective(45f, 1f, 1f, 1000f);
		offCamera.setLocation(new Vector3f(0f, 0f, -5f));
		offCamera.lookAt(new Vector3f(0f, 0f, 0f), Vector3f.UNIT_Y);


		//setup framebuffer's texture
		Texture2D offTex = new Texture2D(512, 512, Format.RGBA8);
		offTex.setMinFilter(Texture.MinFilter.Trilinear);
		offTex.setMagFilter(Texture.MagFilter.Bilinear);

		//setup framebuffer to use texture
		offBuffer.setDepthBuffer(Format.Depth);
		offBuffer.setColorTexture(offTex);

		//set viewport to render to offscreen framebuffer
		myOffscrenViewport.setOutputFrameBuffer(offBuffer);

		// setup framebuffer's scene
		Box boxMesh = new Box(Vector3f.ZERO, 1, 1, 1);
		Material material = assetMgr.loadMaterial("jme3dat/Interface/Logo/Logo.j3m");
		myOffscreenBoxMaterial = material.clone();

		myOffscreenBoxGeom = new Geometry("boxOff", boxMesh);
		myOffscreenBoxGeom.setMaterial(myOffscreenBoxMaterial);
	
		// attach the scene to the viewport to be rendered
		myOffscrenViewport.attachScene(myOffscreenBoxGeom);
		/*java.lang.IllegalStateException: Scene graph is not properly updated for rendering.
     [java] State was changed after rootNode.updateGeometricState() call. 
     [java] Make sure you do not modify the scene from another thread!
		 *  Problem spatial name: boxOff
     [java] 	at com.jme.scene.Spatial.checkCulling(Spatial.java:260)
     [java] 	at com.jme3.renderer.RenderManager.renderSubScene(RenderManager.java:647)
	 com.jme3.renderer.RenderManager.renderScene(RenderManager.java:640)
     [java] 	at com.jme3.renderer.RenderManager.renderViewPort(RenderMateRenderModule}.doRunOnce(seqNum=0)-END
     [java] nager.java:974)
     [java] 	at com.jme3.renderer.RenderManager.render(RenderManager.java:1023)
     [java] 	at com.jme3.app.SimpleApplication.update(SimpleApplication.java:251)
     [java] 	at com.jme3.system.lwjgl.LwjglAbstractDisplay.runLoop(LwjglAbstractDisplay.java:151)
     [java] 	at com.jme3.system.lwjgl.LwjglCanvas.runLoop(LwjglCanvas.java:229)
     [java] 	at com.jme3.system.lwjgl.LwjglAbstractDisplay.run(LwjglAbstractDisplay.java:228)
     [java] 	at java.lang.Thread.run(Thread.java:662)* 
		 */
		myOffscreenBoxGeom.updateLogicalState(tpf);
		myOffscreenBoxGeom.updateGeometricState();
		return offTex;
	}
	
	public boolean isEnabled() { 
		if (myOffscrenViewport != null) {
			return myOffscrenViewport.isEnabled();
		} 
		return false;
	}
	
	public void updateRotatingOffscreenBox_onRendThrd(float tpf) {
		Quaternion q = new Quaternion();

		if (myOffscrenViewport.isEnabled()) {
			myOffscreenBoxRotationAngle += tpf;
			myOffscreenBoxRotationAngle %= FastMath.TWO_PI;
			q.fromAngles(myOffscreenBoxRotationAngle, 0, myOffscreenBoxRotationAngle);

			myOffscreenBoxGeom.setLocalRotation(q);
			// http://hub.jmonkeyengine.org/javadoc/com/jme3/scene/Spatial.html#updateLogicalState(float)
			// updateLogicalState calls the update() method for all controls attached to this Spatial.
			myOffscreenBoxGeom.updateLogicalState(tpf);
			// updateGeometricState updates the lightlist, computes the world transforms, and computes the world 
			// bounds for this Spatial. Calling this when the Spatial is attached to a node will cause undefined 
			// results. User code should only call this method on Spatials having no parent.
			
			// Apparently this should be the *last* thing that happens to this scene graph during a particular
			// OpenGL callback invocation.  So if we're using writeTexture below, it should happen *before* this call.
			myOffscreenBoxGeom.updateGeometricState();
		}
	}
	// This optional additional feature happens to write to the material of the "main" box of the offscreen scene.
	// Entirely separate from the Texture of the offscreen scene itself.
	public void writeTextureToOffscreenBoxMaterial_onRendThrd(Texture2D cameraTex) {
        // Stu:  But should this only be happening on OpenGL thread?
		myOffscreenBoxMaterial.setTexture("ColorMap", cameraTex);
		myOffscreenBoxGeom.setMaterial(myOffscreenBoxMaterial);		
	}	
}
