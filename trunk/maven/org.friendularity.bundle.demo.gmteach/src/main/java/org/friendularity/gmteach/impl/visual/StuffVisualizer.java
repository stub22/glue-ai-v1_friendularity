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
package org.friendularity.gmteach.impl.visual;

import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.goody.dynamic.ShapeAnimator;
import org.cogchar.render.goody.dynamic.VizShape;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.gmteach.api.west.StuffEstimate;

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class StuffVisualizer extends ShapeAnimVisualizer<StuffEstimate> {

	public enum Kind {

		REGULAR,
		MONSTER
	}
	private Kind myKind = Kind.REGULAR;
	
	public StuffVisualizer(HumanoidRenderContext hrc, Kind shapeKind) {
		super(hrc);
		myKind = shapeKind;
	}

	@Override protected void attachSimpleVizObj(StuffEstimate est) {
		if (myKind == Kind.MONSTER) {
			attachMonsterShape(est);
		} else {
			super.attachSimpleVizObj(est);
		}
	}

	public void attachMonsterShape(StuffEstimate est) {
		float initRadius = 5.0f;
		ColorRGBA initColor = ColorRGBA.Green;
		Vector3f basePos = new Vector3f(10.0f, 10.0f, 10.0f);
		myCachedVizObject = new VizShape(est.getIdent(), basePos, initRadius, initColor);
		RenderRegistryClient rrc = getRenderRegistryClient();
		ShapeAnimator sa = getShapeAnimator();
		sa.attachChild_onRendThrd(rrc, myCachedVizObject);
	}
}