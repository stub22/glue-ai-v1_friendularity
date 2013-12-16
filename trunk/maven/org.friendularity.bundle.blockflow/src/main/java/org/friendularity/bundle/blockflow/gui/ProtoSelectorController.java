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
package org.friendularity.bundle.blockflow.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import org.friendularity.bundle.blockflow.engine.BlockflowEngine;

/**
 *
 * @author Annie
 */
class ProtoSelectorController {

	private boolean isDragging = false;
	private BlockflowEngine myEngine;
	private Rectangle blockSelection = new Rectangle();
	
	ProtoSelectorController(BlockflowEngine myEngine) {
		this.myEngine = myEngine;
	}

	boolean mouseClicked(MouseEvent e) {
		if(!e.isAltDown())return false;
		
		return false;
	}

	boolean mousePressed(MouseEvent e) {
		if(!e.isAltDown())return false;
		
		isDragging = true;
		Rectangle curRect = new Rectangle(e.getX(), e.getY(), 1, 1);
		myEngine.getViewport().getBlocksInclusive(curRect, blockSelection);
		myEngine.decorationsChanged();
		return false;
	}

	boolean mouseReleased(MouseEvent e) {
		if(!isDragging)return false;
		
		addPointToSelection(e);
		
		isDragging = false;
		myEngine.decorationsChanged();
		return false;
	}

	boolean mouseEntered(MouseEvent e) {
		return false;
	}

	boolean mouseExited(MouseEvent e) {
		return false;
	}

	boolean mouseDragged(MouseEvent e) {
		if(!isDragging)return false;
		
		addPointToSelection(e);
		myEngine.decorationsChanged();
		
		return false;
	}

	boolean mouseMoved(MouseEvent e) {
		return false;
	}

	boolean mouseWheelMoved(MouseWheelEvent e) {
		return false;
	}

	void paintDecorations(Graphics2D g2) {
		if(!isDragging)return;
		
		Rectangle ul = new Rectangle();
		Rectangle lr = new Rectangle();
		
		myEngine.getViewport().blockLocation(blockSelection.x, blockSelection.y, ul);
		myEngine.getViewport().blockLocation(
				blockSelection.x + blockSelection.width, 
				blockSelection.y + blockSelection.height, lr);
		
		Color c = g2.getColor();
		g2.setStroke(new BasicStroke(3.0f));
		g2.drawRect(ul.x, ul.y, lr.x, lr.y);
		g2.setColor(c);
	}

	private void addPointToSelection(MouseEvent e) {
		Rectangle curRect = new Rectangle(e.getX(), e.getY(), 1, 1);
		Rectangle newBlocks = new Rectangle();
		
		myEngine.getViewport().getBlocksInclusive(curRect, newBlocks);
		
		if(newBlocks.x < blockSelection.x)
		{
			blockSelection.width += blockSelection.x - newBlocks.x;
			blockSelection.x = newBlocks.x;
		}
		else if(blockSelection.x + blockSelection.width < newBlocks.x)
		{
			blockSelection.width = newBlocks.x - blockSelection.x;
		}
		
		if(newBlocks.y < blockSelection.y)
		{
			blockSelection.height += blockSelection.y - newBlocks.y;
			blockSelection.y = newBlocks.y;
		}
		else if(blockSelection.y + blockSelection.width < newBlocks.y)
		{
			blockSelection.height = newBlocks.y - blockSelection.y;
		}		
			
	}
	
}
