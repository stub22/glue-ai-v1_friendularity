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
import org.cogchar.render.goody.dynamic.VizShapeGroup;
import org.cogchar.render.goody.dynamic.VizShape;
import org.friendularity.api.west.ThingEstimate;
import org.friendularity.api.west.StuffEstimate;
import org.cogchar.render.app.humanoid.HumanoidRenderContext;

import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class StuffVisualizer extends SingleShapeVisualizer<StuffEstimate> {

	public enum Kind {

		REGULAR,
		MONSTER
	}
	private Kind myKind = Kind.REGULAR;
	
	public StuffVisualizer(HumanoidRenderContext hrc, VizShapeGroup existingGroup, Kind shapeKind) {
		super(hrc, existingGroup);
		myKind = shapeKind;
	}

	@Override protected void attachSimpleVizObj_onRendThrd(StuffEstimate est) {
		if (myKind == Kind.MONSTER) {
			attachMonsterShape(est);
		} else {
			super.attachSimpleVizObj_onRendThrd(est);
		}
	}

	public void attachMonsterShape(StuffEstimate est) {
		float initRadius = 5.0f;
		ColorRGBA initColor = ColorRGBA.Green;
		Vector3f basePos = new Vector3f(10.0f, 10.0f, 10.0f);
		myCachedVizObject = new VizShape(est.getIdent(), basePos, initRadius, initColor);
		RenderRegistryClient rrc = getRenderRegistryClient();
		VizShapeGroup vsg = getShapeGroup();
		vsg.attachChild_onRendThrd(rrc, myCachedVizObject);
	}
}
