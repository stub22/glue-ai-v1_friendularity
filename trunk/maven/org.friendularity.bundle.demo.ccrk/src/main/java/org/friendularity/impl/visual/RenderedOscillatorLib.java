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

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import org.friendularity.api.west.Oscillator;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class RenderedOscillatorLib {
	
	public static class ColorOscillator extends Oscillator<ColorRGBA> {
		public ColorOscillator(String mExpr, ColorRGBA outColor) { 
			super(mExpr, 4, outColor);
		}
		public ColorOscillator(String mExpr) { 
			this(mExpr, new ColorRGBA());
		}
		
		@Override protected void updateOutObjFromDoublesBuf(ColorRGBA outColor, double[] buffer) {
			outColor.set((float) buffer[0], (float) buffer[1], (float) buffer[2], (float) buffer[3]);
		}
		public ColorRGBA getColor() { 
			return getOutputObject();
		}
	}
	public static class Vec3fOscillator extends Oscillator<Vector3f> {
		public Vec3fOscillator(String mExpr, Vector3f outObj) { 
			super(mExpr, 3, outObj);
		}	
		public Vec3fOscillator(String mExpr) { 
			this(mExpr, new Vector3f());
		}
		@Override protected void updateOutObjFromDoublesBuf(Vector3f outVec3f, double[] buffer) {
			outVec3f.set((float) buffer[0], (float) buffer[1], (float) buffer[2]);
		}
		public Vector3f getVector3f() { 
			return getOutputObject();
		}		
	}
}
