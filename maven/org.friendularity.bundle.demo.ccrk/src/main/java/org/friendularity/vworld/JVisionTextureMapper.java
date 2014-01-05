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
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.render.opengl.optic.MatFactory;
import org.cogchar.render.opengl.optic.TextureFactory;
import org.cogchar.render.opengl.optic.ViewportFacade;
import org.cogchar.render.sys.registry.RenderRegistryClient;

import org.cogchar.render.opengl.scene.DeepSceneMgr;

import org.appdapter.core.log.BasicDebugger;
import org.friendularity.jvision.broker.ImageFlavorNotAvailable;
import org.friendularity.jvision.broker.ImageStreamBroker;

import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.friendularity.jvision.engine.JVisionEngine;

/**
 *
 * @author Owner
 */
public class JVisionTextureMapper extends BasicDebugger implements ImageStreamConsumer {

	// private static final String			TOGGLE_UPDATE = "Toggle Update";

	Texture2D		myLatestTexture;
	
	public void connectToImageStreamBroker() { 
		// This is currently called on the render thread!
		/*
		// The "always" approach recommended in JVision comments appears to lead to a system crash if the 
		// offairConsumer is later replaced with a real consumer
		// (which happens if this method is called before JVision is fully initialized, and then JVision
		// tries to start up for real - boom!)
     [java] # A fatal error has been detected by the Java Runtime Environment:
     [java] #
     [java] #  EXCEPTION_ACCESS_VIOLATION (0xc0000005) at pc=0x000000006dc3a39b, pid=11312, tid=7260		
		// ImageStreamBroker.getDefaultImageStreamBroker().alwaysAddImageStreamConsumer(
		//	JVisionEngine.JVISION_IS_NAME, this);
		// So, now we are back to the deprecated methods:
		*/
		// This appeared to work in limited testing, but presumably involves a race condition with the JVision init.
		// ImageStreamBroker.getDefaultImageStreamBroker().addImageStreamConsumer(JVisionEngine.JVISION_IS_NAME, this);
		// This blocking approach should be reliable, as long as JVision is eventually available.
		getLogger().info("Making blocking connection to JVision using waitAndAddImageStreamConsumer()");
		ImageStreamBroker.getDefaultImageStreamBroker().waitAndAddImageStreamConsumer(JVisionEngine.JVISION_IS_NAME, this);
	}


	// TODO:  We want to minimize the amount of memory allocation going on here
	private Texture2D loadTextureFromImage (BufferedImage buffdImg) { 
		AWTLoader awtLoader = new AWTLoader();
		// CAUTION - this can alter img!   nasty little suprise
		boolean flag_flipY = false;
		Image awtImage = awtLoader.load(buffdImg, flag_flipY);
		Texture2D cameraTex = new Texture2D(awtImage);
		return cameraTex;
	}


	@Override
	public void setConsumedImage(ImageStreamImage visionBuffdImg) {
		try {
			// This is executing on a thread launched by JVision ImageStreamBroker.
			// So, it is not allowed to directly modify the OpenGL scene graph.
			myLatestTexture = loadTextureFromImage(visionBuffdImg.getBufferedImage());
		} catch (ImageFlavorNotAvailable ex) {
			Logger.getLogger(JVisionTextureMapper.class.getName()).log(Level.INFO,
					"JVisionTextureMapper fed with stream not convertable to BufferedImage", ex);
		}
	}
	public Texture2D peekLatestTexture() { 
		return myLatestTexture;
	}
	public Texture2D takeLatestTextureOrNull() { 
		Texture2D taken = myLatestTexture;
		myLatestTexture = null;
		return taken;
	}

	@Override public void setConsumedMessage(String string) {
	}

	@Override public void sourceIsEnding() {
	}
}