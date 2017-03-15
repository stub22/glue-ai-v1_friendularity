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

package org.friendularity.visual.texture;
import com.jme3.material.Material;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.shape.Box;
import com.jme3.texture.Texture;
import com.jme3.texture.Texture2D;

import com.jme3.renderer.RenderManager;
import com.jme3.asset.AssetManager;
import org.cogchar.render.sys.registry.RenderRegistryClient;

import org.cogchar.render.opengl.scene.DeepSceneMgr;

import org.appdapter.core.log.BasicDebugger;
import org.friendularity.visual.texture.OffscreenTextureMapper;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class MagicVisionBoxScene extends BasicDebugger {
	private Geometry					myOnscreenBoxGeom;
	private OffscreenTextureMapper myOTM;
	private	JVisionTextureMapper		myJVTM;


	public void setup_onRendThrd(RenderRegistryClient rrc, float tpf) {
		AssetManager assetMgr = rrc.getJme3AssetManager(null);
		RenderManager renderMgr = rrc.getJme3RenderManager(null);
		//ViewportFacade viewportFacade = rrc.getOpticViewportFacade(null);
		//MatFactory matFact = rrc.getOpticMaterialFacade(null, null);
		//TextureFactory textureFact = rrc.getOpticTextureFacade(null);

		myOTM = new OffscreenTextureMapper();
		
		Texture txtr = myOTM.setupOffscreenView_onRendThrd(renderMgr, assetMgr, tpf);

		myOnscreenBoxGeom = makeOnscreenTexturedBox(txtr, assetMgr);
		DeepSceneMgr dsm = rrc.getSceneDeepFacade(null);

		dsm.attachTopSpatial(myOnscreenBoxGeom);

		getLogger().info("Attached onscreen box, offview enabled=" + myOTM.isEnabled());
	}
	public void setJVisionTextureMapper(JVisionTextureMapper jvtm) {
		myJVTM = jvtm;
	}
	
//	public void simpleInitApp() {
	//	cam.setLocation(new Vector3f(3, 3, 3));
	//	cam.lookAt(Vector3f.ZERO, Vector3f.UNIT_Y);
	public Geometry makeOnscreenTexturedBox(Texture tex, AssetManager assetMgr) {
		//setup main scene
		Geometry boxGeom = new Geometry("boxOn", new Box(Vector3f.ZERO, 20, 20, 20));


		Material mat = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md");
		mat.setTexture("ColorMap", tex);
		boxGeom.setMaterial(mat);
		return boxGeom;
		//	rootNode.attachChild(quad);
		//	inputManager.addMapping(TOGGLE_UPDATE, new KeyTrigger(KeyInput.KEY_SPACE));
		//	inputManager.addListener(this, TOGGLE_UPDATE);
	}
	public void update_onRendThrd(float tpf) {
		
		if (myJVTM != null) {
			Texture2D latestTexture = myJVTM.takeLatestTextureOrNull();
			if (latestTexture != null) {
				myOTM.writeTextureToOffscreenBoxMaterial_onRendThrd(latestTexture);
			}
		}
		// This should happen last because it calls "updateGeometricState()". 
		myOTM.updateRotatingOffscreenBox_onRendThrd(tpf);
	}
}
