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
	
	private ArrayList<BlockflowEngineChangedListener> modelListeners = new ArrayList<BlockflowEngineChangedListener>();
	private BlockflowModel myModel;
	
	public BlockflowEngine()
	{
		myViewport = new BlockViewport();
		myViewport.addViewListener(this);
		myModel = new BlockflowModel();
	}
	
	public BlockViewport getViewport() {
		return myViewport;
	}

	@Override
	public void viewChanged(BlockViewport view) {
		for(Iterator<BlockflowEngineChangedListener>i = modelListeners.iterator() ; i.hasNext(); )
		{
			i.next().engineChanged(this);
		}
	}

	public void addEngineListener(BlockflowEngineChangedListener engineListener) {
		modelListeners.add(engineListener);
	}

	public BlockflowModel getModel() {
		return myModel;
	}

	public void decorationsChanged() {
		for(Iterator<BlockflowEngineChangedListener>i = modelListeners.iterator() ; i.hasNext(); )
		{
			i.next().engineChanged(this);
		}	
	}
}
