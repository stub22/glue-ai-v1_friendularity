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

/**
 *
 * @author Annie
 */
public class ItsBentoBoxesNotBentoTetrisException extends java.lang.Exception {
    private String msg;
	
	public ItsBentoBoxesNotBentoTetrisException(
			int col,
			int row,
			int colsize,
			int rowsize) {
		msg = "Replacement gives nonrectangular shape, nipping " +
				col +
				" - " +
				row +
				" size " +
				colsize +
				" - " +
				rowsize;		
	}

	@Override
	public String getMessage() {
		return msg;
	}

	@Override
	public String getLocalizedMessage() {
		return getMessage();
	}

	
	
}
