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

package org.friendularity.tmpgood.tgflat;

import com.jme3.math.Vector3f;
import org.appdapter.core.name.Ident;
import org.cogchar.render.app.entity.VWorldEntity;
import org.cogchar.render.goody.basic.BasicGoodyCtx;


import java.awt.*;

/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class TG_FlatGoodyWithScreenFracPos extends TG_FlatGoody {
	private	Dimension	myStoredScreenDim;
	private	float		myFracPosX = 0.5f, myFracPosY = 0.5f;
	
	protected TG_FlatGoodyWithScreenFracPos(BasicGoodyCtx bgc, Ident uri) {
		super(bgc, uri);
		// VWorldEntityActionConsumer vweac = GoodyFactory.getTheFactory().getActionConsumer();
		Dimension screenDimension = bgc.getScreenDimension();  //   vweac.getScreenDimension();
		if (screenDimension != null) {
			applyScreenDimension(screenDimension);
		} else {
			getLogger().warn("Cannot find screen dimension.");
		}		
	}
	protected void refreshScreenPos(QueueingStyle qStyle) {
		Dimension sDim = getScreenDim(); 
		// Note that z-coord determines render order of guiNodes
		Vector3f nextAbsPos = new Vector3f (sDim.width * myFracPosX, sDim.height * myFracPosY, 0);
		setScreenPosition(nextAbsPos, qStyle);		
	}
	@Override public final void applyScreenDimension(Dimension screenDimension) {
		myStoredScreenDim = screenDimension;
		refreshScreenPos(QueueingStyle.QUEUE_AND_RETURN);
	}
	protected Dimension getScreenDim() { 
		if (myStoredScreenDim == null) { 
			myStoredScreenDim = new Dimension (640, 480);
		}
		return myStoredScreenDim;
	}
	
	// Position is specified as fraction of screen width/height
	// This is called again every time the screen dimension changes.
	// Questionable assumps about what is now called qStyle:   
	//				Usually we want wait = true, but not for repositioning during window size change
	public void setFractionalPosition(float fracPosX, float fracPosY, VWorldEntity.QueueingStyle qStyle) {
		getLogger().debug("Setting fracPosition: {},{}", fracPosX, fracPosY); 
		myFracPosX = fracPosX;
		myFracPosY = fracPosY;
		refreshScreenPos(qStyle);
	}
}
