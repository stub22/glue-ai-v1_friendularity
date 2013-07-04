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

package org.friendularity.api.west;

import com.jme3.math.*;
import org.appdapter.core.name.Ident;
import org.cogchar.render.sys.registry.RenderRegistryClient;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class ThingEstimate {
	public		Ident						myIdent;
	public		String						myMathExpr;
	public		ShapeAnimator.VizShape		myCachedVizObject;
	public		Object						myCachedMathObject;
	
	public ThingEstimate(Ident id) {
		myIdent = id;
	}
	public void updateFromMathSpace(MathSpace ms) { 
		
	}	
	public void renderAsSillyShape(Visualizer viz) {
		if (myCachedVizObject == null) {
			Vector3f initPos = new Vector3f(35.0f, 35.0f, -5.0f);
			float initRadius = 20.0f;
			ColorRGBA initColor = ColorRGBA.Red;
			myCachedVizObject = new ShapeAnimator.VizShape(myIdent, initPos, initRadius, initColor);
			RenderRegistryClient rrc = viz.getRenderRegistryClient();
			ShapeAnimator sa = viz.getShapeAnimator();
			sa.attachChild_onRendThrd(rrc, myCachedVizObject);
		}
	}
	
	public static abstract class CoordinateFrame {
		public		Ident				myIdent;
		public		CoordinateFrame		myParent;
	}
	public static class Jme3CoordinateFrame extends CoordinateFrame {
		// Vector loc + Quat rot
	}
	public static abstract class Visualizer {
		private		ShapeAnimator	myShapeAnimator;
		public ShapeAnimator getShapeAnimator() { 
			if (myShapeAnimator == null) {
				RenderRegistryClient rrc = getRenderRegistryClient();
				myShapeAnimator = new ShapeAnimator();
				myShapeAnimator.setupMaterials(rrc);
				myShapeAnimator.enable_onRendThrd(rrc);
			}
			return myShapeAnimator;
		}
		protected abstract RenderRegistryClient getRenderRegistryClient();
	}
	public static abstract class MathSpace {
		// Get object or object-set for a given Ident.
	}
}
