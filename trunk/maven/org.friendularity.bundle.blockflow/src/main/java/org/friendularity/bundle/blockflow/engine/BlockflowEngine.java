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
import java.util.ArrayList;
import java.util.Iterator;
import org.friendularity.bundle.blockflow.gui.BlockViewport;
import org.friendularity.bundle.blockflow.gui.ViewListener;

/**
 *
 * @author Annie
 */
public class BlockflowEngine implements ViewListener {

	private BlockViewport myViewport;
	
	private ArrayList<BlockModelChangedListener> views = new ArrayList<BlockModelChangedListener>();
	private BlockflowModel myModel;
	
	public BlockflowEngine()
	{
		myViewport = new BlockViewport();
		myViewport.addViewListener(this);
		myModel = new BlockflowModel();
		
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

	@Override
	public void viewChanged(BlockViewport view) {
		for(Iterator<BlockModelChangedListener>i = views.iterator() ; i.hasNext(); )
		{
			i.next().modelChanged(this);
		}
	}

	public void addView(BlockModelChangedListener view) {
		views.add(view);
	}

	public BlockflowModel getModel() {
		return myModel;
	}
}
