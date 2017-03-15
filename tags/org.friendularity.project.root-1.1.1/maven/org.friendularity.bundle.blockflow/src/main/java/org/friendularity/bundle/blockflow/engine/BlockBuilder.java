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

import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.friendularity.bundle.blockflow.util.QN;
import org.slf4j.LoggerFactory;

import java.util.HashMap;

/**
 * We implement two patterns, Builder and flyweight,
 * A few blocks carry state. Those blocks we make a new instance.
 * But most blocktypes have no state, and hence we can make all references point
 * to the same block instance.
 *
 * then this becomes immutable facts about the blocks only
 *
 * BlockflowModel knows all the mutable stuff,
 * But this class is the fly provider for flyweight pattern
 *
 * This classes 'smarts' are in proto.ttl
 *
 * @author Annie
 */
final class BlockBuilder {
	public static final int BACKGROUND = 0;

	// the background, what fills in places where there's nothing else
	private static BlockishThing bkgnd = new BackgroundBlock();

	// we implement singleton
	private static BlockBuilder defaultBB = null;

	// The RDF model loaded from proto.ttl
	private Model protoModel = null;

	// we lazily instantiate prototypes, and to make
	// sure we only have one we hold them here
	private HashMap<Resource, BlockProto> prototypes = new HashMap<>();

	/**
	 * Return a thing that can fill in a block, based on the constants at top of this class
	 *
	 * @param type type of thing to return, currently only BACKGROUND
	 * @return the BlockishThing
	 */
	public BlockishThing getBlockishThing(int type) {
		if (type == BACKGROUND)
			return bkgnd;
		else
			throw new UnsupportedOperationException("Unknown BlockishThing");
	}

	static BlockBuilder getDefaultBlockBuilder() {
		if (defaultBB == null)
			defaultBB = new BlockBuilder();

		return defaultBB;
	}

	/**
	 * we implement singleton so this is private
	 *
	 * Instantiating the singleton instance loads the model
	 */
	private BlockBuilder() {
		protoModel = ModelFactory.createDefaultModel();
		OSGi_ResourceLoader.getDefaultImageLoader().loadModelFromTurtleResource(
				protoModel, "/ttl/protos.ttl");
	}

	/**
	 * Gets the BlockProto for an RDF class of block
	 *
	 * @param subject an RDF Resource (must be a flo:BlockType)
	 * @return the prototype
	 */
	public BlockProto getPrototype(Resource subject) {
		BlockProto bp = prototypes.get(subject);
		if (bp == null) {
			if (protoModel == null)
				throw new IllegalAccessError("You cannot get prototypes while the model is null");
			bp = new BlockProto(subject);
			prototypes.put(subject, bp);
			return bp;
		} else
			return bp;
	}

	/**
	 * Populate a completely blank BlockflowModel with the default 'new' contents
	 * Typically this is will in turn be overridden by the user's new file template
	 * In short, make the default default model.
	 *
	 * @param bfmodel the model to populate
	 */
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

			if (imageResourceStatement != null &&
					prototypeCoordinateXStatement != null &&
					prototypeCoordinateYStatement != null) {
				LoggerFactory.getLogger(BlockflowEngine.class.getName()).info(subject.toString() + " " +
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
		if (r == null) return null;

		Statement stmt = r.getProperty(
				protoModel.getProperty(QN.flo(propertyQName)));
		if (stmt == null) return null;

		return stmt.getObject();
	}
}
