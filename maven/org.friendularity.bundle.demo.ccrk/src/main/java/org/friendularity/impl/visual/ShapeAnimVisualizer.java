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
package org.friendularity.impl.visual;

import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.cogchar.render.goody.dynamic.ShapeAnimator;
import org.cogchar.render.goody.dynamic.VizShape;
import org.friendularity.api.west.ThingEstimate;

import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class ShapeAnimVisualizer<TE extends ThingEstimate> extends BaseVisualizer<TE> {

	private ShapeAnimator myShapeAnimator;
	public VizShape myCachedVizObject;

	public ShapeAnimVisualizer(HumanoidRenderContext hrc) {
		super(hrc);
	}

	@Override public ShapeAnimator getShapeAnimator() {
		if (myShapeAnimator == null) {
			RenderRegistryClient rrc = getRenderRegistryClient();
			myShapeAnimator = new ShapeAnimator();
			myShapeAnimator.setupMaterials(rrc);
			myShapeAnimator.enable_onRendThrd(rrc);
		}
		return myShapeAnimator;
	}

	@Override public void ensureDisplayed_onRendThrd(TE te, float timePerFrame) {
		if (myCachedVizObject == null) {
			attachSimpleVizObj_onRendThrd(te);
		}
	}

	@Override public void updateDisplay_onRendThrd(TE te, float timePerFrame) {
		if (myCachedVizObject != null) {
			Vector3f updatedPosVec = te.getVisualPos();
			if (updatedPosVec != null) {
				myCachedVizObject.setPosition(updatedPosVec);
			}
			ColorRGBA updatedColor = te.getVisualColor();
			if (updatedColor != null) {
				myCachedVizObject.setColor(updatedColor);
			}
		}
	}

	protected void attachSimpleVizObj_onRendThrd(TE te) {
		getLogger().info("Attaching simple viz-object for {}" + te);
		float initRadius = 5.0f;
		ColorRGBA initColor = ColorRGBA.Red;
		Vector3f basePos = new Vector3f(35.0f, 35.0f, -5.0f);
		myCachedVizObject = new VizShape(te.getIdent(), basePos, initRadius, initColor);
		RenderRegistryClient rrc = getRenderRegistryClient();
		ShapeAnimator sa = getShapeAnimator();
		sa.attachChild_onRendThrd(rrc, myCachedVizObject);
	}
}
