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

package org.friendularity.gmteach.api.west;

/**
 * Stateless transformation from doubles[] to NumType - a useful output type.
 * So far we only define the ability to update an existing NumType, not to create them.
 * @author Stu B. <www.texpedient.com>
 */

public interface NumericMapper<NumType> { 
	/**
	 * Writes into an existing NumType from a double-buffer.  
	 * This function is decoupled from the identity of the node itself.
	 * @param numeric
	 * @param buffer 
	 */	
	public abstract void writeNumericFromDoublesBuf(NumType numeric, double[] buffer);
}
