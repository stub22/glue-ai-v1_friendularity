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
package org.friendularity.jvision.engine;

import java.util.HashMap;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import org.friendularity.jvision.gui.CVChainControl;
import org.friendularity.jvision.gui.FilterBox;

/**
 * Singleton that acts as a builder for new CVChains and their controls
 * @see CVChain 
 * for a discussion of CVChains
 * 
 * @author Annie
 */
public class CVChainManager {
	private static CVChainManager defaultCVManager = null;
	
	private HashMap<String, CVChainControl>cvChainControls = new HashMap<String, CVChainControl>();
	
	public static CVChainManager getDefaultManager() {
		if(defaultCVManager == null)
			defaultCVManager = new CVChainManager();
		
		return defaultCVManager;
	}
	
	private CVChainManager() {
		
		
	}

	public boolean chainExists(String name) {
		return cvChainControls.containsKey(name);
	}

	public void buildChain(FilterBox fb, String name, boolean intermediatesVisible, String source) {
		throw new UnsupportedOperationException("Not supported yet.");
	}
	
			
}
