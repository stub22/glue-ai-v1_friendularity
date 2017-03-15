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
package org.friendularity.tmpgood.tgbit;

import com.jme3.scene.VertexBuffer.Type;
import com.jme3.scene.shape.Box;
import com.jme3.util.BufferUtils;

/**
 * This class customizes the jMonkey Box mesh object with custom texture coordinates for use by BitCube
 * 
 * @author Ryan Biggs
 */


public class TG_BitCubeBox extends Box {
	private static float[] NEW_GEOMETRY_TEXTURE_DATA = {
        1, 0.333333f, 0, 0.333333f, 0, 0.666667f, 1, 0.666667f, // back
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, // right
        1, 0, 0, 0, 0, 0.333333f, 1, 0.333333f, // front
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, // left
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1, // top
        1, 0.666667f, 0, 0.666667f, 0, 1, 1, 1  // bottom
    };

	public TG_BitCubeBox(float x, float y, float z) {
        super(x, y, z);
    }
	
	@Override
	protected void duUpdateGeometryTextures() {
        if (getBuffer(Type.TexCoord) == null){
            setBuffer(Type.TexCoord, 2, BufferUtils.createFloatBuffer(NEW_GEOMETRY_TEXTURE_DATA));
        }
    }
}
