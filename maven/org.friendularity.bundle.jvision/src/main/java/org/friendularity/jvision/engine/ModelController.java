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

import org.friendularity.jvision.gui.FilterBox;

/**
 * Controller around a Jena model
 * handles undo and the dirty bit for load/save
 * 
 * @author Annie
 */
public class ModelController {
	private FilterBox fb;
	private boolean dirty = false;
	
	public ModelController(FilterBox fb) {
		this.fb = fb;
	}
	
	public boolean isDirty() {
		return dirty;
	}
}
