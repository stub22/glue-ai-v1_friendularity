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
package org.friendularity.gmteach.estimate.impl.visual;


import org.appdapter.core.name.Ident;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.cogchar.render.goody.dynamic.VizShapeGroup;
import org.cogchar.render.goody.dynamic.VizShape;
import org.friendularity.gmteach.estimate.api.west.ThingEstimate;

import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;

import org.cogchar.render.app.humanoid.HumanoidRenderContext;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class SingleShapeVisualizer<TE extends ThingEstimate> extends BaseVisualizer<TE> {

	public		VizShape			myCachedVizObject;
		
	private		VizShapeGroup		myShapeGroup;
	
	private		Ident				myOwnedShapeGroupID; // remains null if we are passed an existing group.
	
	public SingleShapeVisualizer(HumanoidRenderContext hrc, Ident ownedShapeGroupID) {
		super(hrc);
		myOwnedShapeGroupID = ownedShapeGroupID;
	}
	public SingleShapeVisualizer(HumanoidRenderContext hrc, VizShapeGroup existingGroup) {
		super(hrc);
		myShapeGroup = existingGroup;
	}
	public SingleShapeVisualizer(BaseVisualizer<?> otherViz, VizShapeGroup existingGroup) {
		super(otherViz);
		myShapeGroup = existingGroup;
	}	

	@Override public VizShapeGroup getShapeGroup() {
		return myShapeGroup;
	}

	@Override public void ensureDisplayed_onRendThrd(TE te, float timePerFrame) {
		if (myShapeGroup == null) {
			RenderRegistryClient rrc = getRenderRegistryClient();
			myShapeGroup = new VizShapeGroup(myOwnedShapeGroupID);
			myShapeGroup.setupMaterials(rrc);
			myShapeGroup.enable_onRendThrd(rrc);
		}
		if (myCachedVizObject == null) {
			attachSimpleVizObj_onRendThrd(te);
		}
	}

	@Override public void updateDisplay_onRendThrd(TE te, float timePerFrame) {
		/*
		if (myCachedVizObject != null) {
			Vector3f updatedPosVec = te.getVisualPos();
			if (updatedPosVec != null) {
				myCachedVizObject.setPosition_onRendThrd(updatedPosVec);
			}
			ColorRGBA updatedColor = te.getVisualColor();
			if (updatedColor != null) {
				myCachedVizObject.setColor_onRendThrd(updatedColor);
			}
			Quaternion updatedDirection = te.getVisualDirection();
			if (updatedDirection != null) {
				myCachedVizObject.setDirection_onRendThrd(updatedDirection);
			}
		}
		*/
	}

	protected void attachSimpleVizObj_onRendThrd(TE te) {
		getLogger().info("Attaching simple viz-object for {}" + te);
		float initRadius = 5.0f;
		ColorRGBA initColor = ColorRGBA.Red;
		Vector3f basePos = new Vector3f(35.0f, 35.0f, -5.0f);
		myCachedVizObject = new VizShape(te.getIdent(), basePos, initRadius, initColor);
		RenderRegistryClient rrc = getRenderRegistryClient();
		VizShapeGroup vsg = getShapeGroup();
		vsg.attachChild_onRendThrd(rrc, myCachedVizObject);
	}
}
