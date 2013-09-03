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

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import java.util.HashMap;

/**
 *
 * @author Annie
 */
final class BlockBuilder {
	public static final int BACKGROUND = 0;

	private static BlockishThing bkgnd = new BackgroundBlock();
	
	private static BlockBuilder defaultBB = null;
	
	private Model theRDFModel = null;
	
	private HashMap<Resource, BlockProtoReprentation>prototypes = new HashMap<Resource, BlockProtoReprentation>();
	
	public BlockishThing getBlockishThing(int type) {
		if(type == BACKGROUND)
			return bkgnd;
		else
			throw new UnsupportedOperationException("Unknown BlockishThing");
	}

	static BlockBuilder getDefaultBlockBuilder() {
		if(defaultBB == null)
			defaultBB = new BlockBuilder();
		
		return defaultBB;
	}
	
	// don't instantiate me
	private BlockBuilder()
	{
		
	}

	public BlockishThing getPrototype(Resource subject) {
		BlockProtoReprentation bp = prototypes.get(subject);
		if(bp == null)
		{
			if(theRDFModel == null)
				throw new IllegalAccessError("You cannot get prototypes while the model is null");
			bp = new BlockProtoReprentation(subject);
			prototypes.put(subject, bp);
			return bp;
		}
		else
			return bp;
	}

	void setRDFModel(Model m) {
		if(m == null)
			throw new IllegalArgumentException("You cannot set BlockBuilders model to null");
		theRDFModel = m;
	}
	
	
	Model getRDFModel() {
		if(theRDFModel == null)
			throw new IllegalArgumentException("You cannot set BlockBuilders model to null");
		return theRDFModel;
	}
			
}
