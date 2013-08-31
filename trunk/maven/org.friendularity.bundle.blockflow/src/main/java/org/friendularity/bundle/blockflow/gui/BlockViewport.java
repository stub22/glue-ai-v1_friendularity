/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.gui;

import java.awt.Rectangle;

/**
 * handles irrotational mapping between 'blocks' which are anisotropic AABB's 
 * and an AABB viewport in pixels.
 * 
 * Block index 0,0 is the UL corner of the origin block.
 * 
 * @author Annie
 */
public class BlockViewport {
	static final int BLOCK_WIDTH  = 128;
	static final int BLOCK_HEIGHT = 96;
	
	// the pixel location of the 
	// UL corner of block 0,0
	int pixelsOfBlockOriginX = 0;
	int pixelsOfBlockOriginY = 0;
	double scale = 1.0d;   // the magnication - so > 1.0 is zoomed way in
	
	private double pixelToBlockX(int pixelX) {
		return   (pixelX - pixelsOfBlockOriginX) / (scale * BLOCK_WIDTH);
	}
	
	private int blockToPixelX(double blockX) {
		return (int) (scale * BLOCK_WIDTH * blockX + pixelsOfBlockOriginX);
	}
	
	private double pixelToBlockY(int pixelY) {
		return   (pixelY - pixelsOfBlockOriginY) / (scale * BLOCK_HEIGHT);
	}
	
	private int blockToPixelY(double blockY) {
		return (int) (scale * BLOCK_HEIGHT * blockY + pixelsOfBlockOriginY);
	}
	
	/**
	 * Get all blocks that intersect viewBounds
	 * includes partially covered blocks
	 * 
	 * @param viewBounds   bounds in pixels
	 * @param blocks       buffer to avoid allocating objects
	 */
	void getBlocksInclusive(Rectangle viewBounds, Rectangle blocks) {
		
		blocks.x = (int) Math.floor(pixelToBlockX(viewBounds.x));
		blocks.y = (int) Math.floor(pixelToBlockY(viewBounds.y));
		// no -1 here because we want the right side of that pixel
		blocks.width = (int) Math.ceil(pixelToBlockX(viewBounds.x + viewBounds.width));
		blocks.height = (int) Math.ceil(pixelToBlockY(viewBounds.y + viewBounds.height));
	}

	/**
	 * get the location of a block named by indices
	 * 
	 * @param i  the x component in blocks
	 * @param j  the y component in blocks
	 * 
	 * @param pos a Rect containing the pixel locations of the edges of the block
	 */
	void blockLocation(int i, int j, Rectangle pos) {
		pos.x = blockToPixelX(i);
		pos.y = blockToPixelY(j);
		pos.width = (int) Math.round(BLOCK_WIDTH * scale);
		pos.height = (int) Math.round(BLOCK_HEIGHT * scale);
	}

	/**
	 * Offset the upper left corner of the viewport by the amount
	 * in pixels. Thus positive dx values move the viewport right,
	 * or the 'paper' left
	 * 
	 * @param dx
	 * @param dy 
	 */
	void offsetPixels(int dx, int dy) {
		pixelsOfBlockOriginX -= dx;
		pixelsOfBlockOriginY -= dy;
	}
	
}
