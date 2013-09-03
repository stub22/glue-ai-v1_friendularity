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

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.ImageObserver;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.friendularity.bundle.blockflow.util.QN;

/**
 *  A block prototype representation
 * 
 * @author Annie
 */
class BlockProtoReprentation extends BlockishThing {

	private String classOfThing = null;
	private Image  componentImage = null;
	
	BlockProtoReprentation(Resource subject) {
		classOfThing = subject.toString();  // don't hold onto Resource, model might change under us
	}

	@Override
	public void paint(Graphics2D g2, Rectangle pos, ImageObserver observer) {
		paintComponent(g2, pos, observer);
		paintDecoration(g2, pos, observer);
	}

	/**
	 * Paint the actual component
	 * 
	 * @param g2
	 * @param pos
	 * @param observer 
	 */
	private void paintComponent(Graphics2D g2, Rectangle pos, ImageObserver observer) {
		componentImage = getComponentImage();
		if (componentImage != null)
			g2.drawImage(componentImage, pos.x, pos.y, pos.width, pos.height, observer);
	}

	private void paintDecoration(Graphics2D g2, Rectangle pos, ImageObserver observer) {
		g2.setColor(Color.red);
		g2.drawLine(pos.x, pos.y, pos.x + pos.width, pos.y + pos.height);
	}
	
	private Image getComponentImage() {
		if(componentImage != null)
			return componentImage;
		
		//  more than slightly bogus but it's what we're doing
		
		Model m = BlockBuilder.getDefaultBlockBuilder().getRDFModel();
		Resource r = m.getResource(classOfThing);
		if (r == null)return null;
		
		Statement imageResourceStatement = r.getProperty(
			m.getProperty(QN.flo("imageResource")));
		if (imageResourceStatement == null)return null;
		
		String resourceName = imageResourceStatement.getObject().asLiteral().getString();
		if (resourceName == null)return null;
		try {
			componentImage = OSGi_ResourceLoader.getDefaultImageLoader().getImageResource(resourceName);
		} catch (IOException ex) {
			Logger.getLogger(BlockProtoReprentation.class.getName()).log(Level.SEVERE, 
					"Resource" + resourceName + " is missing", ex);
			return null;
		}
		
		return componentImage;
	}

			
	
}
