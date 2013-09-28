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
package org.friendularity.bundle.bento.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

/**
 *
 * @author Annie
 */
class BentoBox extends BentoPlugin {

	public BentoBox(BentoPlugin center) {
		this.add(center, BorderLayout.CENTER);
	    center.init();
	}

	/**
	 * Do a bento command - this must be called from the event dispatch thread
	 * 
	 * @param source  source of the command - what handled the right menu
	 * @param actionCommand - essentially, the text of the menu
	 */
	void doCommand(BentoPlugin source, String actionCommand) {
		
		// TODO currently only handles case wehre we've never split before
		Component parent = source.getParent();
		
		if(actionCommand.equals(BentoPlugin.HTWO_MENU))
		{
			BentoPlugin cv = new CameraViewer();

			remove(source);
			add(
					new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
						source, 
						cv)
					);
			cv.init();

			source.invalidate();
			cv.invalidate();
			this.validate();
			this.repaint();
		}
	}
	
}
