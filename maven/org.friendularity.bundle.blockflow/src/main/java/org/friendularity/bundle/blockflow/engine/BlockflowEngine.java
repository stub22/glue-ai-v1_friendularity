/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.engine;

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
		
	}
	
	public BlockViewport getViewport() {
		return myViewport;
	}
	
}
