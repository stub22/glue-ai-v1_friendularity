/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.gui;

import java.awt.Rectangle;

/**
 *
 * @author Annie
 */
public class BlockViewport {
	static final int BLOCK_WIDTH  = 128;
	static final int BLOCK_HEIGHT = 96;
	
	/**
	 * Get all blocks that intersect viewBounds
	 * includes partially covered blocks
	 * 
	 * @param viewBounds   bounds in pixels
	 * @param blocks       buffer to avoid allocating objects
	 */
	void getBlocksInclusive(Rectangle viewBounds, Rectangle blocks) {
		blocks.x = viewBounds.x / BLOCK_WIDTH;
		blocks.y = viewBounds.y / BLOCK_HEIGHT;
		blocks.height = (viewBounds.y + viewBounds.height - 1 + 127) / BLOCK_HEIGHT - blocks.y;
		blocks.width = (viewBounds.x + viewBounds.width - 1 + 127) / BLOCK_WIDTH - blocks.x;
		
	}

	void blockLocation(int i, int j, Rectangle pos) {
		pos.x = i * BLOCK_WIDTH;
		pos.y = j * BLOCK_HEIGHT;
		pos.width = BLOCK_WIDTH;
		pos.height = BLOCK_HEIGHT;
	}
	
}
