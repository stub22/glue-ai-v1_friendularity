/**
 * This is the missing license file
 */
package org.friendularity.bundle.blockflow.engine;

import org.friendularity.bundle.blockflow.gui.BlockViewport;
import org.friendularity.bundle.blockflow.gui.ViewListener;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * @author Annie
 */
public class BlockflowEngine implements ViewListener {

	private BlockViewport myViewport;

	private ArrayList<BlockflowEngineChangedListener> modelListeners = new ArrayList<>();
	private BlockflowModel myModel;

	public BlockflowEngine() {
		myViewport = new BlockViewport();
		myViewport.addViewListener(this);
		myModel = new BlockflowModel();
	}

	public BlockViewport getViewport() {
		return myViewport;
	}

	@Override
	public void viewChanged(BlockViewport view) {
		for (Iterator<BlockflowEngineChangedListener> i = modelListeners.iterator(); i.hasNext(); ) {
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
		for (Iterator<BlockflowEngineChangedListener> i = modelListeners.iterator(); i.hasNext(); ) {
			i.next().engineChanged(this);
		}
	}
}
