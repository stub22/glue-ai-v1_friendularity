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
package org.friendularity.bundle.bento.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/**
 *   A Swing component consisting of a resizeable grid of components, some of which can
 * be bigger than one cell
 * 
 * @author Annie
 */
public class MergeGrid extends JPanel {
	static final int SEPARATOR_WIDTH = 10;
	static final int SEPARATOR_HEIGHT = 10;
	static final int SOME_MINIMAL_CELL_DIM = 4;
	
	// points are col,row
	private HashMap<Point, MergeGridEntry>myCells = new HashMap<Point, MergeGridEntry>();
	
	private ArrayList<Float>columns = new ArrayList<Float>();
	private ArrayList<Float>rows = new ArrayList<Float>();
	
	private ArrayList<HorBentoSplitter>colsplitters = new ArrayList<HorBentoSplitter>();
	private ArrayList<VertBentoSplitter>rowsplitters = new ArrayList<VertBentoSplitter>();
	
	private MergeGridGlassPane theGlass;
	private boolean createColumnMode = false;
	
	public MergeGrid()
	{
		this.setLayout(new MergeGridLayout());
		theGlass = new MergeGridGlassPane();
		
		this.add(theGlass);
	}

	@Override
	public boolean isOptimizedDrawingEnabled() {
		return false; // ensures that z order happens properly
	}
	

	int getNumColumns() {
		return columns.size();
	}
	
	int getNumRows() {
		return rows.size();
	}
	
	int getWidth(int i) {
		return (int)(columns.get(i).floatValue());		
	}
	
	int getHeight(int i) {
		return (int)(rows.get(i).floatValue());
	}
	
	int getColumnAt(int x)
	{
		float myx = 0.0f;
		for(int i = 0 ; i < columns.size() ; i++)
		{
			if (myx <= x && x < myx + columns.get(i) + MergeGrid.SEPARATOR_WIDTH )
				return i;
			myx += columns.get(i) + MergeGrid.SEPARATOR_WIDTH;
		}
		
		throw new IllegalArgumentException("pixel x=" + x + " is not in a column");
	}
	
	int getRowAt(int y)
	{
		float myy = 0.0f;
		for(int i = 0 ; i < rows.size() ; i++)
		{
			if (myy <= y && y < myy + rows.get(i) + MergeGrid.SEPARATOR_HEIGHT )
				return i;
			myy += rows.get(i) + MergeGrid.SEPARATOR_HEIGHT;
		}
		
		throw new IllegalArgumentException("pixel y=" + y + " is not in a row");
	}

	void addColumn(int i, int width) {
		columns.add(i, new Float((float)width));
		HorBentoSplitter s = new HorBentoSplitter();
		this.add(s);
		colsplitters.add(i, s);
		this.revalidate();
	}

	void addRow(int i, int height) {
		rows.add(i, new Float((float)height));
		VertBentoSplitter s = new VertBentoSplitter();
		this.add(s);
		rowsplitters.add(i, s);
		this.revalidate();
	}

	void setCell(JComponent component, int col, int row, int colsize, int rowsize) throws ItsBentoBoxesNotBentoTetrisException {
		if(col < 0 || col + colsize > columns.size())
			throw new IllegalArgumentException(
					"attempt to set cell in columns " + 
					col +
					" - " +
					(col + colsize) +
					" when there are only " +
					columns.size() +
					" columns"
					);
		if(row < 0 || row + rowsize > rows.size())
			throw new IllegalArgumentException(
					"attempt to set cell in rows " +
					row +
					" - " +
					(row + rowsize) +
					" when there are only " +
					rows.size() +
					" rows");
		
		// blow up if this is gonna make a bad bentobox
		for(Iterator<Point>i = myCells.keySet().iterator() ; i.hasNext() ; )
		{
			Point p = i.next();
			MergeGridEntry mge = myCells.get(p);
			
			if(mge.clashesWith(col, row, colsize, rowsize))
				throw new ItsBentoBoxesNotBentoTetrisException(
						mge.col,
						mge.row,
						mge.colsize,
						mge.rowsize);
		}
		
		// zap anything we totally cover
		for(Iterator<Point>i = myCells.keySet().iterator() ; i.hasNext() ; )
		{
			Point p = i.next();
			MergeGridEntry mge = myCells.get(p);
			
			if (mge.coveredBy(col, row, colsize, rowsize))
				this.removeMGE(mge);
		}
		
		this.add(component);
		MergeGridEntry mge = new MergeGridEntry(component, col, row, colsize, rowsize);
		myCells.put(mge.getPoint(), mge);
		this.revalidate();
	}

	private void removeMGE(MergeGridEntry mge) {
		myCells.remove(mge.getPoint());
		this.remove(mge.component);
		this.revalidate();
	}

	JComponent getCellAt(int col, int row) {
		MergeGridEntry mge = myCells.get(new Point(col, row));
		if (mge == null)return null;
		return mge.component;
	}

	HorBentoSplitter getColSplitter(int col) {
		return colsplitters.get(col);
	}

	VertBentoSplitter getRowSplitter(int row) {
		return rowsplitters.get(row);
	}

	void stretchWidth(float widthRatio) {
		for(int i = 0 ; i < columns.size() ; i++)
		{
			columns.set(i, new Float(columns.get(i) * widthRatio));
		}
		
	}

	void stretchHeight(float heightRatio) {
		for(int i = 0 ; i < rows.size() ; i++)
		{
			rows.set(i, new Float(
					rows.get(i) * heightRatio));
		}

	}

	Dimension sizeAt(int col, int row) {
		MergeGridEntry mge = myCells.get(new Point(col, row));
		return new Dimension(mge.colsize, mge.rowsize);
	}

	/**
	 * We always want to answer the question.
	 * 
	 * @param aThis
	 * @return 
	 */
	boolean isLastRowOrColumnSplitter(Object aThis) {
		if(aThis == null)
			return false;
		else if (aThis instanceof HorBentoSplitter) {
			if(colsplitters.size() < 1)return false;

			return colsplitters.get(colsplitters.size() - 1) == aThis;
		} else if (aThis instanceof VertBentoSplitter) {
			if(rowsplitters.size() < 1) return false;

			return rowsplitters.get(rowsplitters.size() - 1) == aThis;			
		} else return false;
	}
	
	int indexOfHorSplitter(HorBentoSplitter aThis) {
		int i = colsplitters.indexOf(aThis);
		if (i >= 0)return i;
		
		throw new IllegalArgumentException("Attempt to find index of HorBentoSplitter not in MergeGrid");
	}

	int indexOfVertSplitter(VertBentoSplitter aThis) {
		int i = rowsplitters.indexOf(aThis);
		if (i >= 0)return i;
		
		throw new IllegalArgumentException("Attempt to find index of VertBentoSplitter not in MergeGrid");
	}

	boolean okToDuplicateLeft(HorBentoSplitter aThis) {
		return  columns.get(indexOfHorSplitter(aThis)) > SEPARATOR_WIDTH + SOME_MINIMAL_CELL_DIM * 2;
	}

	boolean okToDuplicateRight(HorBentoSplitter aThis) {
		if(isLastRowOrColumnSplitter(aThis))
			return false;
		return  columns.get(1 + indexOfHorSplitter(aThis)) > SEPARATOR_WIDTH + SOME_MINIMAL_CELL_DIM * 2;
	}

	HorBentoSplitter duplicateLeft(HorBentoSplitter aThis) {
		int i = this.indexOfHorSplitter(aThis);
		if(columns.get(i) < SOME_MINIMAL_CELL_DIM + SEPARATOR_WIDTH)
			throw new IllegalArgumentException("Trying to make too narrow a column");
		
		int width = (int)(columns.get(i) - SOME_MINIMAL_CELL_DIM - SEPARATOR_WIDTH);
		
		columns.set(i, new Float(SOME_MINIMAL_CELL_DIM));
		// Oh fiddles - this is all wrong - we need to be like shift dragging a prim, where the new one
		// is left (only if we're goign left, depressingly!)
		this.addColumn(i, width);
		return colsplitters.get(i);
	}

	int getNumCells() {
		return myCells.size();
	}

	MergeGridGlassPane getGlassPane() {
		return theGlass;
	}

	boolean interactWithGlassPane() {
		return createColumnMode;
	}

	Component getNonGlassComponentAt(Point epoint) {
		for(int i = 0 ; i < this.getComponentCount() ; i++)
		{
			Component c = this.getComponent(i);
			
			Point cpoint = SwingUtilities.convertPoint(this, epoint , c);
			if(c.contains(cpoint) &&
			  !(c instanceof MergeGridGlassPane))return c;
		}
		return null;
	}

	/**
	 * Insert a column here
	 * 
	 * @param x 
	 * 
	 * @throws BadPositionForAddedRowOrColumn if we can't add the column
	 */
	void insertColAt(int x) throws BadPositionForAddedRowOrColumn {
		if(x < 0)
			throw new BadPositionForAddedRowOrColumn(x);
		if(x > this.getWidth() - 2 * MergeGrid.SEPARATOR_WIDTH - MergeGrid.SOME_MINIMAL_CELL_DIM)
			throw new BadPositionForAddedRowOrColumn(x);
		
		float colx = 0.0f;
		
		for(int i = 0 ; i < columns.size() ; i++)
		{
			if (colx + MergeGrid.SOME_MINIMAL_CELL_DIM <= x && 
				colx + columns.get(i) >= x + MergeGrid.SEPARATOR_WIDTH + MergeGrid.SOME_MINIMAL_CELL_DIM)
			{
				float colwas = columns.get(i);
				
				float leftside = x - colx;
				float rightside = columns.get(i) - leftside - MergeGrid.SEPARATOR_WIDTH;
				columns.set(i, leftside);
				columns.add(i + 1, rightside);
				HorBentoSplitter h = new HorBentoSplitter();
				colsplitters.add(i+1, h);
				this.add(h);
				this.revalidate();
				return;
			}
			colx += columns.get(i);
			colx += MergeGrid.SEPARATOR_WIDTH;
		}
		// no location works, we throw
		throw new BadPositionForAddedRowOrColumn(x);
	}

	/**
	 * Insert a row here
	 * 
	 * @param y
	 * 
	 * @throws BadPositionForAddedRowOrColumn if we can't add the column
	 */
	void insertRowAt(int y) throws BadPositionForAddedRowOrColumn {
		if(y < 0)
			throw new BadPositionForAddedRowOrColumn(y);
		if(y > this.getHeight() - 2 * MergeGrid.SEPARATOR_WIDTH - MergeGrid.SOME_MINIMAL_CELL_DIM)
			throw new BadPositionForAddedRowOrColumn(y);
		
		float rowy = 0.0f;
		
		for(int i = 0 ; i < rows.size() ; i++)
		{
			if (rowy + MergeGrid.SOME_MINIMAL_CELL_DIM <= y && 
				rowy + rows.get(i) >= y + MergeGrid.SEPARATOR_HEIGHT + MergeGrid.SOME_MINIMAL_CELL_DIM)
			{
				float upperside = y - rowy;
				float lowerside = rows.get(i) - upperside - MergeGrid.SEPARATOR_HEIGHT;
				rows.set(i, upperside);
				rows.add(i + 1, lowerside);
				VertBentoSplitter v = new VertBentoSplitter();
				rowsplitters.add(i+1, v);
				this.add(v);
				this.revalidate();
				return;
			}
			rowy += rows.get(i);
			rowy += MergeGrid.SEPARATOR_HEIGHT;
		}
		// no location works, we throw
		throw new BadPositionForAddedRowOrColumn(y);
	}
	
	int resizeColumns(int index, int delta) {
		int newdelta;
		
		if (delta == 0)
			return 0;
		else if (delta > 0)
		{
			newdelta = (int)Math.min(delta, 
					columns.get(index + 1) - MergeGrid.SOME_MINIMAL_CELL_DIM);			
		} else {
			newdelta = (int)Math.max(delta,
					MergeGrid.SOME_MINIMAL_CELL_DIM - columns.get(index));
		}
		
		columns.set(index, columns.get(index) + newdelta);
		columns.set(index + 1, columns.get(index + 1) - newdelta);
		revalidate();
		return newdelta;
	}


	int resizeRows(int index, int delta) {
		int newdelta;
		
		if (delta == 0)
			return 0;
		else if (delta > 0)
		{
			newdelta = (int)Math.min(delta, 
					rows.get(index + 1) - MergeGrid.SOME_MINIMAL_CELL_DIM);			
		} else {
			newdelta = (int)Math.max(delta,
					MergeGrid.SOME_MINIMAL_CELL_DIM - rows.get(index));
		}
		
		rows.set(index, rows.get(index) + newdelta);
		rows.set(index + 1, rows.get(index + 1) - newdelta);
		revalidate();
		
		return newdelta;
	}
	
	int getSumOfColSizes() {
		int w = 0;
		
		for(int i = 0 ; i < columns.size() ; i++)
		{
			w +=  Math.floor(columns.get(i));
		}
		
		return w;
	}

	int getSumOfRowSizes() {
		int h = 0;
		
		for(int i = 0 ; i < rows.size() ; i++)
		{
			h += Math.floor(rows.get(i));
		}
		
		return h;
	}

	void removeCell(JComponent aThis) {
		for(Iterator<Point>i = myCells.keySet().iterator() ; i.hasNext() ; )
		{
			Point p = i.next();
			MergeGridEntry mge = myCells.get(p);
			
			if(mge.component == aThis)
			{
				this.remove(aThis);
				myCells.remove(p);
				revalidate();
				repaint();
				return;
			}
		}
	}

	void attemptDeleteColumn(int col) {
		if(columns.size() < 2)
			return;  // can't delete last column
		
		float amountToHandOut = (columns.get(col) + MergeGrid.SEPARATOR_WIDTH) / (columns.size() - 1);
		
		HashMap<Point, MergeGridEntry>newCells = new HashMap<Point, MergeGridEntry>();
		
		for(Iterator<Point>i = myCells.keySet().iterator() ; i.hasNext() ; )
		{
			Point p = i.next();
			MergeGridEntry mge = myCells.get(p);
			
			// it's on left
			if(mge.col + mge.colsize - 1 < col)
				newCells.put(p, mge);
			// spans into or past col
			else if (mge.col < col && mge.col + mge.colsize - 1 >= col)
			{
				mge.colsize--;
				newCells.put(p, mge);
			}
			// its' on right
			else if (mge.col > col)
			{
				mge.col -= 1;
				
				newCells.put(new Point(p.x - 1, p.y), mge);
			}
			// it's to be removed
			else if (mge.col == col && mge.colsize == 1)
			{
				this.remove(mge.component);
			}
			// this should be all
			else
			{
				Logger.getLogger(MergeGrid.class.getName()).log(Level.SEVERE, 
						"unhandled case deleting column " + col + " from mge.col = " + mge.col +
						" mge.colsize = " + mge.colsize);
			}
		}
		
		myCells = newCells;
		HorBentoSplitter sp = this.getColSplitter(col);
		colsplitters.remove(sp); // ANNIE - just did this, off to lunch with Shakti
		this.remove(sp);

		columns.remove(col);
		
		
		for(int i = 0 ; i < columns.size() ; i++)
			columns.set(i, columns.get(i) + amountToHandOut);
		
		revalidate();
	}

	void attemptDeleteRow(int row) {
		if(rows.size() < 2)
			return;  // can't delete last row
		
		HashMap<Point, MergeGridEntry>newCells = new HashMap<Point, MergeGridEntry>();
		
		float amountToHandOut = (rows.get(row) + MergeGrid.SEPARATOR_HEIGHT) / (rows.size() - 1);
		
		for(Iterator<Point>i = myCells.keySet().iterator() ; i.hasNext() ; )
		{
			Point p = i.next();
			MergeGridEntry mge = myCells.get(p);
			
			// it's above
			if(mge.row + mge.rowsize - 1 < row)
				newCells.put(p, mge);
			// spans into or past row
			else if (mge.row < row && mge.row + mge.rowsize - 1 >= row)
			{
				mge.rowsize--;
				newCells.put(p, mge);
			}
			// its' below
			else if (mge.row > row)
			{
				mge.row -= 1;
				
				newCells.put(new Point(p.x, p.y - 1), mge);
			}
			// it's to be removed
			else if (mge.row == row && mge.rowsize == 1)
			{
				this.remove(mge.component);
			}
			// this should be all
			else
			{
				Logger.getLogger(MergeGrid.class.getName()).log(Level.SEVERE, 
						"unhandled case deleting row " + row + " from mge.row = " + mge.row +
						" mge.rowsize = " + mge.rowsize);
			}
		}
		
		myCells = newCells;
		VertBentoSplitter sp = this.getRowSplitter(row);
		rowsplitters.remove(sp);
		this.remove(sp);
		rows.remove(row);
		
		for(int i = 0 ; i < rows.size() ; i++)
			rows.set(i, rows.get(i) + amountToHandOut);
		
		revalidate();
	}
	
	private static class MergeGridEntry {
		int col;
		int row;
		int colsize;
		int rowsize;
		JComponent component;
		
		public MergeGridEntry(JComponent component, int col , int row, int colsize, int rowsize) {
			this.col = col;
			this.row = row;
			this.colsize = colsize;
			this.rowsize = rowsize;
			this.component = component;
		}

		private boolean clashesWith(int acol, int arow, int acolsize, int arowsize) {
			return (
					(col < acol && col + colsize > acol) ||
					     (col  < acol + acolsize && col + colsize >= acol + acolsize)) &&
					((row < arow && row + rowsize > arow) ||
					     (row < arow + arowsize && row + rowsize >= arow + arowsize));
		}

		private boolean coveredBy(int acol, int arow, int acolsize, int arowsize) {
			return (col >= acol) && 
				   (col + colsize <= acol + acolsize) &&
				   (row >= arow) && 
				   (row + rowsize <= arow + arowsize);
		}

		private Point getPoint() {
			return new Point(col, row);
		}
	}
	
}
