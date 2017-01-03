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

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import org.appdapter.core.name.Ident;
import org.cogchar.render.app.entity.GoodyActionExtractor;
import org.cogchar.render.goody.basic.BasicGoodyCtx;


/**
 * This class defines general purpose 2d text display goodies
 *
 * @author Ryan Biggs <rbiggs@hansonrobokind.com>
 */


public class TG_ParagraphGoody extends TG_FlatGoody {
	
	private static final ColorRGBA DEFAULT_COLOR = ColorRGBA.Black;
	private static final float DEFAULT_SCALE = 1.0f;
	private static final String DEFAULT_TEXT = "No text set";
	
	private TG_FlatGoodyTextElement myTextElement;
	
	public TG_ParagraphGoody(BasicGoodyCtx bgc, Ident uri, Vector3f position, Float scale,
							 ColorRGBA color, String text) {
		super(bgc, uri);

		if (color == null) {
			color = DEFAULT_COLOR;
		}
		if (scale == null) {
			scale = DEFAULT_SCALE;
		}
		if (text == null) {
			text = DEFAULT_TEXT;
		}
		myTextElement = new TG_FlatGoodyTextElement(bgc.getRRC());
		myTextElement.setContentText(text);
		myTextElement.setUniformScaleFactor(scale, QueueingStyle.QUEUE_AND_RETURN);
		myTextElement.setColor(color);
		setPosition(position, QueueingStyle.QUEUE_AND_RETURN);
	}
	
	@Override public void applyAction(GoodyActionExtractor ga, QueueingStyle qStyle) {
		super.applyAction(ga, qStyle); // Applies "standard" set and move actions
		switch (ga.getKind()) {
			case SET : {
				String text = ga.getText();
				if (text != null) {
					myTextElement.setContentText(text);
				}
				break;
			}
		}
	}

	@Override protected Node getFlatGoodyNode() {
		return myTextElement.getTextNode();
	}


}
