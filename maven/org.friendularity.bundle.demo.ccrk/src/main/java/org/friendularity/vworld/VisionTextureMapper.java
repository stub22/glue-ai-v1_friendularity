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
import org.friendularity.jvision.broker.ImageStreamBroker;

import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.engine.JVisionEngine;

/**
 *
 * @author Owner
 */
public class VisionTextureMapper extends BasicDebugger implements ImageStreamConsumer {

	private static final String TOGGLE_UPDATE = "Toggle Update";
	private Geometry myOffscreenBoxGeom, myOnscreenBoxGeom;
	private ViewPort myOffscrenViewport;
	private Material mCameraMaterial = new Material();
	private float angle = 0;

	public void setup(RenderRegistryClient rrc) {
		AssetManager assetMgr = rrc.getJme3AssetManager(null);
		RenderManager renderMgr = rrc.getJme3RenderManager(null);
		ViewportFacade viewportFacade = rrc.getOpticViewportFacade(null);
		MatFactory matFact = rrc.getOpticMaterialFacade(null, null);
		TextureFactory textureFact = rrc.getOpticTextureFacade(null);

		Texture txtr = setupOffscreenView(renderMgr, assetMgr);

		myOnscreenBoxGeom = makeOnscreenTexturedBox(txtr, assetMgr);
		DeepSceneMgr dsm = rrc.getSceneDeepFacade(null);

		dsm.attachTopSpatial(myOnscreenBoxGeom);

		getLogger().info("Attached onscreen box, offview enabled=" + myOffscrenViewport.isEnabled());
	}

	public Texture setupOffscreenView(RenderManager renderMgr, AssetManager assetMgr) {
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
		mCameraMaterial = material.clone();

		myOffscreenBoxGeom = new Geometry("boxOff", boxMesh);
		//	myOffscreenBoxGeom.setMaterial(material);
		myOffscreenBoxGeom.setMaterial(mCameraMaterial);
		// TBD ANNIE this is probably horribly the wrong place for this
		ImageStreamBroker.getDefaultImageStreamBroker().waitAndAddImageStreamConsumer(
			JVisionEngine.JVISION_IS_NAME, this);

		// attach the scene to the viewport to be rendered
		myOffscrenViewport.attachScene(myOffscreenBoxGeom);

		return offTex;
	}

//	public void simpleInitApp() {
	//	cam.setLocation(new Vector3f(3, 3, 3));
	//	cam.lookAt(Vector3f.ZERO, Vector3f.UNIT_Y);
	public Geometry makeOnscreenTexturedBox(Texture tex, AssetManager assetMgr) {
		//setup main scene
		Geometry boxGeom = new Geometry("boxOn", new Box(Vector3f.ZERO, 20, 20, 20));

		// 	Texture offTex = setupOffscreenView();

		Material mat = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md");
		mat.setTexture("ColorMap", tex);
		boxGeom.setMaterial(mat);
		return boxGeom;
		//	rootNode.attachChild(quad);
		//	inputManager.addMapping(TOGGLE_UPDATE, new KeyTrigger(KeyInput.KEY_SPACE));
		//	inputManager.addListener(this, TOGGLE_UPDATE);
	}

	public void simpleUpdate(float tpf) {
		Quaternion q = new Quaternion();

		if (myOffscrenViewport.isEnabled()) {
			angle += tpf;
			angle %= FastMath.TWO_PI;
			q.fromAngles(angle, 0, angle);

			myOffscreenBoxGeom.setLocalRotation(q);
			// http://hub.jmonkeyengine.org/javadoc/com/jme3/scene/Spatial.html#updateLogicalState(float)
			// updateLogicalState calls the update() method for all controls attached to this Spatial.
			myOffscreenBoxGeom.updateLogicalState(tpf);
			// updateGeometricState updates the lightlist, computes the world transforms, and computes the world 
			// bounds for this Spatial. Calling this when the Spatial is attached to a node will cause undefined 
			// results. User code should only call this method on Spatials having no parent.
			myOffscreenBoxGeom.updateGeometricState();
		}
	}

	@Override
	public void setConsumedImage(BufferedImage img) {
/*
		AWTLoader awtLoader = new AWTLoader();
		// CAUTION - this can alter img!
		// nasty little suprise
		Image cameraImage = awtLoader.load(img, false);
		Texture2D cameraTex = new Texture2D(cameraImage);

		mCameraMaterial.setTexture("ColorMap", cameraTex);
		myOffscreenBoxGeom.setMaterial(mCameraMaterial);
*/
	}

	@Override
	public void setConsumedMessage(String string) {
	}

	@Override
	public void sourceIsEnding() {
	}
}
