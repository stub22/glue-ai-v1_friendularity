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

import java.awt.*;
import java.util.HashMap;

/**
 * @author Annie
 */
public class BlockflowModel {
	private HashMap<Point, BlockishThing> myThings;

	public BlockflowModel() {
		myThings = new HashMap<>();

		BlockBuilder.getDefaultBlockBuilder().populateNewBlockflowModel(this);
	}

	public BlockishThing getThing(int i, int j) {
		BlockishThing bt = myThings.get(new Point(i, j));
		if (bt == null)
			return BlockBuilder.getDefaultBlockBuilder().getBlockishThing(
					BlockBuilder.getDefaultBlockBuilder().BACKGROUND);
		else
			return bt;
	}

	void setThingAtLocation(int i, int j, BlockishThing prototype) {
		myThings.put(new Point(i, j), prototype);
	}
}
