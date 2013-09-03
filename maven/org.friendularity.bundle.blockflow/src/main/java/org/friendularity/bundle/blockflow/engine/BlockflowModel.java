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
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.vocabulary.VCARD;
import java.awt.Point;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.friendularity.bundle.blockflow.util.QN;

/**
 *
 * @author Annie
 */
public class BlockflowModel {
	private HashMap<Point, BlockishThing>myThings;
	private Model currentProtoModel;
	
	public BlockflowModel()
	{
		myThings = new HashMap<Point, BlockishThing>();
				// create an empty Model
		currentProtoModel = ModelFactory.createDefaultModel();
		 OSGi_ResourceLoader.getDefaultImageLoader().loadModelFromTurtleResource(
				 currentProtoModel, "/ttl/protos.ttl");
		 
		BlockBuilder.getDefaultBlockBuilder().setRDFModel(currentProtoModel);
		indexModel();
	}

	public BlockishThing getThing(int i, int j) {
		BlockishThing bt = myThings.get(new Point(i,j));
		if (bt == null)
			return BlockBuilder.getDefaultBlockBuilder().getBlockishThing(
					BlockBuilder.getDefaultBlockBuilder().BACKGROUND);
		else
			return bt;
	}

	private void indexModel() {
		// list the statements in the Model
		ResIterator iter = currentProtoModel.listSubjectsWithProperty(
				currentProtoModel.getProperty(QN.rdf("type")),
				currentProtoModel.getResource(QN.flo("BlockType")));

		// print out the predicate, subject and object of each statement
		while (iter.hasNext()) {
			Resource subject = iter.next();
			Statement imageResourceStatement = subject.getProperty(
					currentProtoModel.getProperty(QN.flo("imageResource")));
			Statement prototypeCoordinateXStatement = 
					subject.getProperty(
						currentProtoModel.getProperty(QN.flo("prototypeCoordinateX")));
			Statement prototypeCoordinateYStatement = 
					subject.getProperty(
						currentProtoModel.getProperty(QN.flo("prototypeCoordinateY")));			

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
				myThings.put(new Point(prototypeCoordinateXStatement.getObject().asLiteral().getInt(),
						prototypeCoordinateYStatement.getObject().asLiteral().getInt()),
						BlockBuilder.getDefaultBlockBuilder().getPrototype(subject));
			}
		}
	}
}
