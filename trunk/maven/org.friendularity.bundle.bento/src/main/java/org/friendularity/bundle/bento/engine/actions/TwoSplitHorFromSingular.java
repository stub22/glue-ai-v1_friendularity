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
package org.friendularity.bundle.bento.engine.actions;

import org.friendularity.bundle.bento.engine.BentoAction;
import org.friendularity.bundle.bento.gui.BentoPlugin;
import org.friendularity.bundle.bento.gui.CameraViewer;

/**
 *
 * @author Annie
 */
public class TwoSplitHorFromSingular implements BentoAction {

	@Override
	public boolean handle(BentoPlugin source, String actionCommand) {
		
		if (!actionCommand.equals(BentoPlugin.HTWO_MENU))return false;
		source.getBentoBox().add(new CameraViewer(), 0.5f);
		source.getBentoBox().revalidate();
		
		return true;
		// DONE TO HERE
		/*
		if(actionCommand.equals(BentoPlugin.HTWO_MENU))
		{
			if(parent instanceof BentoBox)
			{
				BentoPlugin cv = new CameraViewer();

				remove(source);
				add(
						new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
							source, 
							cv)
						);
				cv.init();

				source.revalidate();
				cv.revalidate();
				this.revalidate();
				this.repaint();
			} else if (parent instanceof JSplitPane && 
					((JSplitPane)parent).getLeftComponent() == source){
				JSplitPane parentSplit = ((JSplitPane)parent);
				BentoPlugin cv = new CameraViewer();
				
				JSplitPane newSplit = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
							source, 
							cv);

				parentSplit.setLeftComponent(newSplit);
				
			//	newSplit.setDividerLocation(0.5);
			//	parentSplit.setDividerLocation(0.66);
				cv.init();

				source.revalidate();
				cv.revalidate();
				newSplit.revalidate();
				this.revalidate();
				this.repaint();				
			} else if (parent instanceof JSplitPane &&
					((JSplitPane)parent).getRightComponent() == source) {
				JSplitPane parentSplit = ((JSplitPane)parent);
				BentoPlugin cv = new CameraViewer();
				
				JSplitPane newSplit = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
							source, 
							cv);

				parentSplit.setRightComponent(newSplit);
				newSplit.setDividerLocation(0.5);
				parentSplit.setDividerLocation(0.66);
				cv.init();

				source.revalidate();
				cv.revalidate();
				this.revalidate();
				this.repaint();					
			}
		} else if (actionCommand.equals(BentoPlugin.REMOVE_MENU))
		{
			// this is just all horribly wrong
		}
		*/
	}
	
}
