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
package org.friendularity.bundle.blockflow.engine;

import com.hp.hpl.jena.rdf.model.Resource;

import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.image.ImageObserver;
import java.io.IOException;

/**
 * A block prototype
 *
 * Part of BlockBuilder's flies.
 *
 * @author Annie
 */
class BlockProto extends BlockishThing {

	private String classOfThing = null;
	// we lazy init these
	private Image componentImage = null;
	private int componentWidth = -1;
	private int componentHeight = -1;

	BlockProto(Resource subject) {
		classOfThing = subject.toString();  // don't hold onto Resource, model might change under us
	}

	@Override
	public void paint(Graphics2D g2, Rectangle pos, ImageObserver observer) {
		paintDecoration(g2, pos, observer);
		paintComponent(g2, pos, observer);
	}

	/**
	 * Paint the actual component
	 */
	private void paintComponent(Graphics2D g2, Rectangle pos, ImageObserver observer) {
		componentImage = getComponentImage();

		if (componentImage != null) {
			int w = getComponentCellWidth();
			int h = getComponentCellHeight();

			g2.drawImage(componentImage, pos.x, pos.y, w * pos.width, h * pos.height, observer);
		}
	}

	private void paintDecoration(Graphics2D g2, Rectangle pos, ImageObserver observer) {
		int x = pos.x;
		int y = pos.y;
		int w = getComponentCellWidth() * pos.width;
		int h = getComponentCellHeight() * pos.height;

		float alpha = 0.1f;

		while (h > 0 && w > 0 && alpha >= 0.0f) {
			g2.setColor(new Color(1.0f, 1.0f, 0.0f, alpha));
			g2.fillOval(x, y, w, h);

			alpha += 0.1f;
			if (alpha > 0.5f) alpha = 0.5f;
			x += 10;
			y += 10;
			w -= 20;
			h -= 20;
		}

	}

	private Image getComponentImage() {
		if (componentImage != null)
			return componentImage;

		//  more than slightly bogus but it's what we're doing

		String resourceName = BlockBuilder.getDefaultBlockBuilder()
				.getPropertyOf(classOfThing, "imageResource").asLiteral().getString();

		if (resourceName == null) return null;
		try {
			componentImage = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource(resourceName);
		} catch (IOException ex) {
			LoggerFactory.getLogger(BlockProto.class.getName()).error(
					"Resource" + resourceName + " is missing", ex);
			return null;
		}

		return componentImage;
	}

	private int getComponentCellWidth() {
		if (componentWidth > 0)
			return componentWidth;

		//  more than slightly bogus but it's what we're doing

		try {
			componentWidth = BlockBuilder.getDefaultBlockBuilder()
					.getPropertyOf(classOfThing, "cellWidth").asLiteral().getInt();
		} catch (NullPointerException npe) {
			componentWidth = 1;
		}

		return componentWidth;
	}

	private int getComponentCellHeight() {
		if (componentHeight > 0)
			return componentHeight;

		//  more than slightly bogus but it's what we're doing

		try {
			componentHeight = BlockBuilder.getDefaultBlockBuilder()
					.getPropertyOf(classOfThing, "cellHeight").asLiteral().getInt();
		} catch (NullPointerException npe) {
			componentHeight = 1;
		}

		return componentHeight;
	}


}
