/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.engine;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.vocabulary.VCARD;
import org.friendularity.bundle.blockflow.gui.BlockViewport;

/**
 *
 * @author Annie
 */
public class BlockflowEngine {

	private BlockViewport myViewport;
	
	public BlockflowEngine()
	{
		myViewport = new BlockViewport();
		
		testJena();
	}
	
	public BlockViewport getViewport() {
		return myViewport;
	}
	
	void testJena() {
		// some definitions
		String personURI    = "http://www.pathwayslms.com/JohnSmith";
		String fullName     = "John Smith";

		// create an empty Model
		Model model = ModelFactory.createDefaultModel();

		// create the resource
		Resource johnSmith = model.createResource(personURI);

		// add the property
		 johnSmith.addProperty(VCARD.FN, fullName);
	}
}
