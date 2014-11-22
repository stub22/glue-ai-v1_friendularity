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
package org.friendularity.jvision.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Stroke;
import javax.swing.ButtonModel;
import javax.swing.JCheckBox;
import javax.swing.JToggleButton;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 *
 * @author Annie
 */
class CVChainControlBorder implements Border {
   
	private static final Insets insets = new Insets(8, 8, 8, 8);
	private static final Stroke selectStroke = new BasicStroke(4, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER);
	private static final Stroke  unselectStroke = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL);
	
	private CVChainControl myControl;
	
	public CVChainControlBorder(CVChainControl aControl) {
		this.myControl = aControl;
	}

	@Override
	public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
		Graphics2D g2 = (Graphics2D)g;
		Stroke os = g2.getStroke();
		Color oc = g2.getColor();
		
		if(myControl.isSelected())
		{
			g2.setStroke(selectStroke);
			g2.setColor(Color.black);
		} else {
			g2.setStroke(unselectStroke);
			g2.setColor(Color.gray);			
		}
		g2.drawRect(x+4, y+4 , Math.max(0, width- 8), Math.max(0, height - 8));
		
		g2.setStroke(os);
		g2.setColor(oc);
	}

	@Override
	public Insets getBorderInsets(Component c) {
		return insets;
	}

	@Override
	public boolean isBorderOpaque() {
		return false;
	}	
}
