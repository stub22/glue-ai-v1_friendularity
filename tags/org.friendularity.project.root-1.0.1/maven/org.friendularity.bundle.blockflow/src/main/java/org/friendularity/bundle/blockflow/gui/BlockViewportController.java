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

import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.blockflow.engine.BlockflowEngine;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.friendularity.bundle.blockflow.util.ProportionUtils;

/**
 *
 * @author Annie
 */
public class BlockViewportController {
	private static final double ZOOM_SPEED  = 0.03d;
	private static final double ZOOM_ALL_WAY_OUT = 0.0625d;
	public static final double ZOOM_ALL_WAY_IN = 1.5d;
	
	private BlockflowEngine myEngine;
	
	private Cursor myGrabber = null;
	
	private int global_drag_x;
	private int global_drag_y;
	
	public BlockViewportController(BlockflowEngine engine)
	{
		myEngine = engine;
		myGrabber = OSGi_ResourceLoader.getDefaultImageLoader().getCursorResource(
				"/img/grabbercursor.png", 
				16, 16, 
				"RobokindGrabber");
	}
	
	public boolean mouseWheelMoved(MouseWheelEvent e) {
		if(!e.isControlDown() && !e.isAltDown() && !e.isMetaDown())
		{
			double zoom = ProportionUtils.cage(ZOOM_ALL_WAY_OUT,
					myEngine.getViewport().getZoom() *  (1.0d + ZOOM_SPEED * e.getWheelRotation()),
					ZOOM_ALL_WAY_IN);
			
			myEngine.getViewport().setZoom(zoom, e.getX(), e.getY());

			return true;		
		}
		return false;
	}

	boolean mousePressed(MouseEvent e) {
		if(e.getButton() == MouseEvent.BUTTON2)
		{
			global_drag_x = e.getXOnScreen();
			global_drag_y = e.getYOnScreen();
			return true;
		}
		return false;
	}

	boolean mouseReleased(MouseEvent e) {
		if(e.getButton() == MouseEvent.BUTTON2)
		{
			myEngine.getViewport().offsetPixels(
					global_drag_x - e.getXOnScreen(), 
					global_drag_y - e.getYOnScreen());
			global_drag_x = e.getXOnScreen();
			global_drag_y = e.getYOnScreen();
			e.getComponent().setCursor(Cursor.getDefaultCursor());
			return true;
		}
		return false;
	}

	boolean mouseDragged(MouseEvent e) {
		int onmask = MouseEvent.BUTTON2_DOWN_MASK;
		int offmask = MouseEvent.CTRL_DOWN_MASK | MouseEvent.SHIFT_DOWN_MASK | MouseEvent.ALT_DOWN_MASK | 
			MouseEvent.BUTTON1_DOWN_MASK | MouseEvent.BUTTON3_DOWN_MASK;
		
		if ((e.getModifiersEx() & (onmask | offmask)) == onmask) {
			myEngine.getViewport().offsetPixels(
					global_drag_x - e.getXOnScreen(), 
					global_drag_y - e.getYOnScreen());
			global_drag_x = e.getXOnScreen();
			global_drag_y = e.getYOnScreen();
			e.getComponent().setCursor(myGrabber);
			return true;
		}
		return false;
	}
	
}
