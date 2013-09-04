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
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import java.awt.Point;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.friendularity.bundle.blockflow.util.QN;

/**
 *
 * TODO - refactor this so it's the owner of the model off proto.ttl
 * 
 * and change the name of prototypeCoordinateX to defaultPrototypeCoordinateX
 * then this becomes immutable facts about the blocks only
 * 
 * BlockflowModel knows all the mutable stuff, including the locations of BlockProtoRepresentation instances
 * 
 * But this class is the fly provider for flyweight pattern
 * 
 * @author Annie
 */
final class BlockBuilder {
	public static final int BACKGROUND = 0;

	private static BlockishThing bkgnd = new BackgroundBlock();
	
	private static BlockBuilder defaultBB = null;
	
	private Model protoModel = null;
	
	private HashMap<Resource, BlockProto>prototypes = new HashMap<Resource, BlockProto>();
	
	
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
				// create an empty Model
		protoModel = ModelFactory.createDefaultModel();
		OSGi_ResourceLoader.getDefaultImageLoader().loadModelFromTurtleResource(
				 protoModel, "/ttl/protos.ttl");
	}

	public BlockishThing getPrototype(Resource subject) {
		BlockProto bp = prototypes.get(subject);
		if(bp == null)
		{
			if(protoModel == null)
				throw new IllegalAccessError("You cannot get prototypes while the model is null");
			bp = new BlockProto(subject);
			prototypes.put(subject, bp);
			return bp;
		}
		else
			return bp;
	}			

	void populateNewBlockflowModel(BlockflowModel bfmodel) {
		// list the statements in the Model
		ResIterator iter = protoModel.listSubjectsWithProperty(
				protoModel.getProperty(QN.rdf("type")),
				protoModel.getResource(QN.flo("BlockType")));

		// print out the predicate, subject and object of each statement
		while (iter.hasNext()) {
			Resource subject = iter.next();
			Statement imageResourceStatement = subject.getProperty(
					protoModel.getProperty(QN.flo("imageResource")));
			Statement prototypeCoordinateXStatement = 
					subject.getProperty(
						protoModel.getProperty(QN.flo("defaultPrototypeCoordinateX")));
			Statement prototypeCoordinateYStatement = 
					subject.getProperty(
						protoModel.getProperty(QN.flo("defaultPrototypeCoordinateY")));			

		    if(imageResourceStatement != null && 
					prototypeCoordinateXStatement != null &&
					prototypeCoordinateYStatement != null)
			{
				Logger.getLogger(BlockflowEngine.class.getName()).log(Level.INFO, 
						subject.toString() + " " +
						imageResourceStatement.getObject().toString() + " (" +
						prototypeCoordinateXStatement.getObject().asLiteral().getInt() + ", " +
						prototypeCoordinateYStatement.getObject().asLiteral().getInt() + ")"
						);
				bfmodel.setThingAtLocation(
						prototypeCoordinateXStatement.getObject().asLiteral().getInt(),
						prototypeCoordinateYStatement.getObject().asLiteral().getInt(),
						BlockBuilder.getDefaultBlockBuilder().getPrototype(subject));
			}
		}
	}

	RDFNode getPropertyOf(String classOfThing, String propertyQName) {
		Resource r = protoModel.getResource(classOfThing);
		if (r == null)return null;
		
		Statement stmt = r.getProperty(
			protoModel.getProperty(QN.flo(propertyQName)));
		if (stmt == null)return null;
		
		return stmt.getObject();
	}
}
