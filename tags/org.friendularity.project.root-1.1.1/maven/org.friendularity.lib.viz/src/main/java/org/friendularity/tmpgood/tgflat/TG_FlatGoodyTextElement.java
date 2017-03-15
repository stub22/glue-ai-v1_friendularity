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

package org.friendularity.tmpgood.tgflat;

import com.jme3.font.BitmapText;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import org.appdapter.core.log.BasicDebugger;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.cogchar.render.sys.task.Queuer.QueueingStyle;

// import org.cogchar.render.sys.registry.RenderRegistryClient;

/**
 * "x,y locations of guiNode children are the x,y pixel on screen and z controls the render order.
 *  guiNode at 0,0 = lower left of the screen."
 * @author Ryan Biggs <rbiggs@hansonrobokind.com>
 */


public class TG_FlatGoodyTextElement extends BasicDebugger {
	
	// Note that *lower* left corner is (0,0)
	private Vector3f					myScreenPosRelToParent = new Vector3f(); 

	private BitmapText					myOverlayText;
	private RenderRegistryClient myRenderRegCli;
	private	float						myScale = 1.0f;
	private ColorRGBA					myColor;
	private String						myContent = "No Content Set";
	
	public TG_FlatGoodyTextElement(RenderRegistryClient aRenderRegCli) {
		myRenderRegCli = aRenderRegCli;
	}
	public void setScreenPosRelToParent(Vector3f sprtp, QueueingStyle qStyle) {
		myScreenPosRelToParent = sprtp;
	}
	
	public void setContentText(String goodyText) {
		myContent = goodyText;
		if (myOverlayText != null) {
			myOverlayText.setText(goodyText);
		} 
	}
	
	public void setUniformScaleFactor(Float scale, QueueingStyle qStyle) {
		getLogger().debug("Setting 2d Goody scale to {}", scale); 
		if (scale != null) {
			myScale = scale;
			if (myOverlayText != null) {
					// Stu 2013-10-20  do we need: setAbsolutePosition(myScreenPosRelToParent, QueueingStyle.QUEUE_AND_RETURN);
				myOverlayText.setSize(myOverlayText.getFont().getCharSet().getRenderedSize()* scale);
			}
		}
	}
	
	public void setColor(ColorRGBA color) {
		if (color != null) {
			myColor = color;
			if (myOverlayText != null) {
				myOverlayText.setColor(color);
			}
		}
	}
	public BitmapText getTextNode() { 
		if (myOverlayText == null) {
			myOverlayText = myRenderRegCli.getSceneTextFacade(null).getScaledBitmapText(myContent, myScale);
		}
		return myOverlayText;
	}
}
