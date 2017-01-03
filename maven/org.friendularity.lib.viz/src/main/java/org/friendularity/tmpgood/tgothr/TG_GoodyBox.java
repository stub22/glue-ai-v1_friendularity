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
package org.friendularity.tmpgood.tgothr;

import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.shape.Box;
import org.appdapter.core.name.Ident;
import org.cogchar.render.goody.basic.BasicGoodyCtx;
import org.cogchar.render.goody.basic.BasicGoodyEntity;

/**
 *
 * @author Ryan Biggs
 */
public class TG_GoodyBox extends BasicGoodyEntity {

    private final ColorRGBA DEFAULT_COLOR = ColorRGBA.LightGray;

    public TG_GoodyBox(BasicGoodyCtx bgc, Ident boxUri, Vector3f position, Quaternion rotation,
					   ColorRGBA color, final Vector3f newScale) {
        super(bgc, boxUri);
        // This check is shared with BitCube and BitBox and should be factored out:
        if (newScale == null) {
            getLogger().warn("No size specified for TG_GoodyBox, defaulting to size = 1");
        } else if (Math.abs(newScale.length() - 0.0f) < 0.001f) {
            getLogger().warn("TG_GoodyBox being created with zero size!");
        } else {
            setVectorScale(newScale, QueueingStyle.QUEUE_AND_RETURN);
        }
        setPosition(position, QueueingStyle.QUEUE_AND_RETURN);
        /**
         * The size is 1, but the scale will change its size.
         */
        Mesh goodyBox = new Box(1, 1, 1);
        if (color == null) {
            color = DEFAULT_COLOR;
        }
        addGeometry(goodyBox, color, rotation);
    }
}
