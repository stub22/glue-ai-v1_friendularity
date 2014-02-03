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
import com.jme3.math.Quaternion;
import org.friendularity.api.struct.MathExprNode;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class VisualMathExprLib {
	
	public static class ColorExprNode extends MathExprNode<ColorRGBA> {
		public ColorExprNode(String mExpr, ColorRGBA initColorObj) { 
			super(mExpr, 4, initColorObj);
		}
		public ColorExprNode(String mExpr) { 
			this(mExpr, new ColorRGBA());
		}
		// TODO: Instead of this method, factor into a Color-NumberMapper and set it as our delegate. 
		@Override protected void writeAnyNumericFromAnyDoublesBuf(ColorRGBA outColor, double[] buffer) {
			outColor.set((float) buffer[0], (float) buffer[1], (float) buffer[2], (float) buffer[3]);
		}
		
		public ColorRGBA getColor() { 
			return getOutputObject();
		}
	}
	public static class Vec3fExprNode extends MathExprNode<Vector3f> {
		public Vec3fExprNode(String mExpr, Vector3f initVectObj) { 
			super(mExpr, 3, initVectObj);
		}	
		public Vec3fExprNode(String mExpr) { 
			this(mExpr, new Vector3f());
		}
		// TODO: Instead of this method, factor into a Color-NumberMapper and set it as our delegate.
		@Override protected void writeAnyNumericFromAnyDoublesBuf(Vector3f outVec3f, double[] buffer) {
			outVec3f.set((float) buffer[0], (float) buffer[1], (float) buffer[2]);
		}
		public Vector3f getVector3f() { 
			return getOutputObject();
		}		
	}
	
	public static class QuaternionExprNode extends MathExprNode<Quaternion> {
		public QuaternionExprNode(String mExpr, Quaternion initQuatObj) { 
			super(mExpr, 4, initQuatObj);
		}
		public QuaternionExprNode(String mExpr) { 
			this(mExpr, new Quaternion());
		}		
	}
}
