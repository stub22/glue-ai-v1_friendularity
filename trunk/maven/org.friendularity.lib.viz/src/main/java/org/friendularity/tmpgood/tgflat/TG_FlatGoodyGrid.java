/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.tmpgood.tgflat;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * A grid defines a local coordinate space called GridSpace where bot-left = 
 */

public class TG_FlatGoodyGrid {
	public float	SpaceMinX = -100.0f, SpaceMaxX = 100.0f, SpaceMinY=-100.0f, SpaceMaxY = 100.0f;
	public	int		myCellCountH, myCellCountV;
	
	public float getSpaceWidth() { 
		return SpaceMaxX - SpaceMinX;
	}
	public float getSpaceHeight() { 
		return SpaceMaxX - SpaceMinX;
	}	
	public boolean getSpaceFlipVert() { 
		return true;
	}
}
