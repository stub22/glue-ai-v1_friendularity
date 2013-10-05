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

import org.friendularity.bundle.bento.engine.BentoAction;
import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.ListIterator;
import org.friendularity.bundle.bento.engine.actions.TwoSplitHorFromSingular;

/**
 *
 * @author Annie
 */
public class BentoBox extends BentoPlugin {

	public enum Direction {
		SINGULAR,
		HORIZONTAL,
		VERTICAL
	};
	
	private Direction myDirection = Direction.SINGULAR;
	
	private static final ArrayList<BentoAction>theActions = new ArrayList<BentoAction>();
	
	static {
		theActions.add(new TwoSplitHorFromSingular());
	}
			
	public BentoBox(BentoPlugin center) {
		this.add(center, BorderLayout.CENTER);
		this.setLayout(new HorizontalBentoLayoutManager());
		
	    center.init(this);  // 8cD fun with leaking this! Kernighan & Plauger forever!
	}

	/**
	 * Do a bento command - this must be called from the event dispatch thread
	 * 
	 * @param source  source of the command - what handled the right menu
	 * @param actionCommand - essentially, the text of the menu
	 */
	void doCommand(BentoPlugin source, String actionCommand) {
		
		for(ListIterator<BentoAction> i = theActions.listIterator(); 
				i.hasNext() ; )
		{
			BentoAction ba = i.next();
			
			if(ba.handle(source, actionCommand))return;
		}
	}
	
}
