/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.engine;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.VCARD;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bundle.blockflow.gui.BlockViewport;
import org.friendularity.bundle.blockflow.gui.ViewListener;
import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;
import org.friendularity.bundle.blockflow.util.QN;

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
		 
		 OSGi_ResourceLoader.getDefaultImageLoader().loadModelFromTurtleResource(model, "/ttl/protos.ttl");
		 
		 Resource  dlbend = model.getResource(QN.flo("dlbend"));
		 
		 Property imageResourceProp = model.getProperty(QN.flo("imageResource"));
		 
		 // returns null if doesn't have this prop
		 Statement imageResourceName = dlbend.getProperty(imageResourceProp);
		 
		 imageResourceName.getObject().toString();
		 
		 System.out.println("WAHOOOOOOOOO " + imageResourceName.getObject().toString());
		
		// list the statements in the Model
		ResIterator iter = model.listSubjectsWithProperty(
				model.getProperty(QN.rdf("type")),
				model.getResource(QN.flo("BlockType")));

		// print out the predicate, subject and object of each statement
		while (iter.hasNext()) {
			Resource subject = iter.next();
			Statement imageResourceStatement = subject.getProperty(
					model.getProperty(QN.flo("imageResource")));
			Statement prototypeCoordinateXStatement = 
					subject.getProperty(
						model.getProperty(QN.flo("prototypeCoordinateX")));
			Statement prototypeCoordinateYStatement = 
					subject.getProperty(
						model.getProperty(QN.flo("prototypeCoordinateY")));			

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
			}
		}
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
