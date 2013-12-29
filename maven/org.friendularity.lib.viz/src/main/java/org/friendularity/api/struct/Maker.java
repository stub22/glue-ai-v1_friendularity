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

package org.friendularity.api.struct;

/**
 * @author Stu B. <www.texpedient.com>
 */

public interface Maker<Elem> {
	public Elem makeOne();
	// The copying op is often easier than make(), since the size+shape of source+target can be inspected.
	public void shallowCopyContents(Elem source, Elem target);
	
	// public Elem[] makeArray(int size);
}
