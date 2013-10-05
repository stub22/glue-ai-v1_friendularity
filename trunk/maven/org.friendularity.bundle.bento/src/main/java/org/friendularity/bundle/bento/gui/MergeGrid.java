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

import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import javax.swing.JComponent;
import javax.swing.JPanel;

/**
 *   A Swing component consisting of a resizeable grid of components, some of which can
 * be bigger than one cell
 * 
 * @author Annie
 */
public class MergeGrid extends JPanel {
	static int SEPARATOR_WIDTH = 10;
	static int SEPARATOR_HEIGHT = 10;
	
	// points are col,row
	private HashMap<Point, MergeGridEntry>myCells = new HashMap<Point, MergeGridEntry>();
	
	private ArrayList<Float>columns = new ArrayList<Float>();
	private ArrayList<Float>rows = new ArrayList<Float>();
	
	private ArrayList<HorBentoSplitter>colsplitters = new ArrayList<HorBentoSplitter>();
	private ArrayList<VertBentoSplitter>rowsplitters = new ArrayList<VertBentoSplitter>();
	
	public MergeGrid()
	{
		this.setLayout(new MergeGridLayout());
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
		return myCells.get(new Point(col, row)).component;
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
			return (col < acol && col + colsize >= acol) ||
					(col  < acol + acolsize && col + colsize >= acol + acolsize) ||
					(row < arow && row + rowsize > arow) ||
					(row < arow + arowsize && row + rowsize >= arow + arowsize);
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
