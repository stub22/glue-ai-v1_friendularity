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
package org.friendularity.model.test;

import com.jme3.app.SimpleApplication;
import com.jme3.asset.plugins.FileLocator;
import com.jme3.font.BitmapText;
import com.jme3.light.DirectionalLight;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import java.io.File;

/**
 *
 * @author Ryan Biggs <rbiggs@skyriversoftware.com>
 */
public class ModelViewer extends SimpleApplication {

	private static final String HUD_MESSAGE = "Press Esc to exit and load new model";
	private String modelPath;
	private File modelFile;

	ModelViewer(File file) {
		modelFile = file;
	}

	@Override
	public void simpleInitApp() {
		setDisplayFps(false);
		//setDisplayStatView(false);
		flyCam.setMoveSpeed(50);
		cam.setLocation(new Vector3f(0f, 30f, 60f));
		cam.lookAtDirection(new Vector3f(0f, -0.17f, -1f), Vector3f.UNIT_Y);
		viewPort.setBackgroundColor(ColorRGBA.LightGray);
		// Load a model from test data (OgreXML + material + texture)
		assetManager.registerLocator(modelFile.getParent(), FileLocator.class);
		Spatial testModel = assetManager.loadModel(modelFile.getName());
		//testModel.scale(0.25f, 0.25f, 0.25f);
		//testModel.setLocalTranslation(0.0f, -5.0f, -2.0f);
		rootNode.attachChild(testModel);
		// You must add a light to make the model visible
		DirectionalLight sun = new DirectionalLight();
		sun.setDirection(new Vector3f(0f, -0.7f, -1.0f));
		rootNode.addLight(sun);
		// For more light
		DirectionalLight sun2 = new DirectionalLight();
		sun2.setDirection(new Vector3f(0f, -0.7f, 1.0f));
		rootNode.addLight(sun2);
		DirectionalLight sun3 = new DirectionalLight();
		sun3.setDirection(new Vector3f(1f, -0.7f, 0f));
		rootNode.addLight(sun3);
		DirectionalLight sun4 = new DirectionalLight();
		sun4.setDirection(new Vector3f(-1f, -0.7f, 0f));
		rootNode.addLight(sun4);
		BitmapText infoText = new BitmapText(guiFont);
		infoText.setSize(24f);
		infoText.setColor(ColorRGBA.Black);
		infoText.setText(HUD_MESSAGE);
		infoText.setLocalTranslation(400, infoText.getLineHeight(), 0); // position
		guiNode.attachChild(infoText);
	}

	@Override
	public void destroy() {
		super.destroy();
		ModelPathDialog.start(null);
	}
}
