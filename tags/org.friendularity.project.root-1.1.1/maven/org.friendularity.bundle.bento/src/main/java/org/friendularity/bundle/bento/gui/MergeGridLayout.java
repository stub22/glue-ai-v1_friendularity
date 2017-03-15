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

import org.slf4j.LoggerFactory;

import java.awt.*;

import javax.swing.*;

/**
 * @author Annie
 */
class MergeGridLayout implements LayoutManager2,
		java.io.Serializable {

	public MergeGridLayout() {

	}

	@Override
	public void addLayoutComponent(Component comp, Object constraints) {
	}

	@Override
	public float getLayoutAlignmentX(Container target) {
		return 0.0f;
	}

	@Override
	public float getLayoutAlignmentY(Container target) {
		return 0.0f;
	}

	/**
	 * Invalidates the layout, indicating that if the layout manager
	 * has cached information it should be discarded.
	 */
	@Override
	public void invalidateLayout(Container target) {

	}

	@Override
	public void addLayoutComponent(String name, Component comp) {

	}

	@Override
	public void removeLayoutComponent(Component comp) {

	}

	@Override
	public Dimension maximumLayoutSize(Container target) {
		Dimension d = new Dimension(0, 0);

		if (!(target instanceof MergeGrid))
			throw new IllegalArgumentException("MergeGridLayout requires a MergeGrid");

		MergeGrid mg = (MergeGrid) target;

		for (int i = 0; i < mg.getNumColumns(); i++) {
			d.width += mg.getWidth(i);
		}
		d.width += mg.getNumColumns() * MergeGrid.SEPARATOR_WIDTH;
		for (int i = 0; i < mg.getNumRows(); i++) {
			d.height += mg.getHeight(i);
		}
		d.height += mg.getNumRows() * MergeGrid.SEPARATOR_HEIGHT;
		return d;
	}

	@Override
	public Dimension preferredLayoutSize(Container parent) {
		return maximumLayoutSize(parent);
	}

	@Override
	public Dimension minimumLayoutSize(Container parent) {
		return maximumLayoutSize(parent);
	}

	Dimension oldSize = new Dimension(0, 0);

	// this is where we do the real work. We do some computation
	// and then call setbounds on each component in the parent
	// we don't actually dink with parent
	@Override
	public void layoutContainer(Container parent) {
		if (!(parent instanceof MergeGrid))
			throw new IllegalArgumentException("MergeGridLayout requires a MergeGrid");

		MergeGrid mg = ((MergeGrid) parent);

		int oldSumOfColSizes = mg.getSumOfColSizes();
		int oldSumOfRowSizes = mg.getSumOfRowSizes();

		if (oldSize.width != 0 &&
				oldSize.height != 0 &&
				!mg.getSize().equals(oldSize)) {
			int widthSum = parent.getWidth() - mg.getNumColumns() * MergeGrid.SEPARATOR_WIDTH;
			int heightSum = parent.getHeight() - mg.getNumRows() * MergeGrid.SEPARATOR_HEIGHT;

			if (widthSum > 0 &&
					heightSum > 0 &&
					(widthSum != oldSumOfColSizes || heightSum != oldSumOfRowSizes)) {
				float widthRatio = widthSum / (float) oldSumOfColSizes;
				float heightRatio = heightSum / (float) oldSumOfRowSizes;

				mg.stretchWidth(widthRatio);
				mg.stretchHeight(heightRatio);

				oldSize = parent.getSize();
			}
		} else {
			oldSize = parent.getSize();
		}

		Point p = new Point(0, 0);

		// the z order index for the components
		int curZOrder = mg.getNumCells() +
				mg.getNumColumns() + mg.getNumRows() + // # of splitters
				1 - 1;  // 1 for glasspane, -1 for last index one less than count

		p.x = 0;
		for (int col = 0; col < mg.getNumColumns(); col++) {
			p.y = 0;
			for (int row = 0; row < mg.getNumRows(); row++) {
				JComponent c = mg.getCellAt(col, row);
				if (c != null) {
					Dimension d = mg.sizeAt(col, row);
					Dimension size = new Dimension(0, 0);

					for (int cc = col; cc < col + d.width; cc++)
						size.width += mg.getWidth(cc);
					size.width += MergeGrid.SEPARATOR_WIDTH * (d.width - 1);
					for (int rr = row; rr < row + d.height; rr++)
						size.height += mg.getHeight(rr);
					size.height += MergeGrid.SEPARATOR_HEIGHT * (d.height - 1);

					c.setLocation(p);
					c.setSize(size);
					mg.setComponentZOrder(c, curZOrder--);
				}

				p.y += mg.getHeight(row);
				p.y += MergeGrid.SEPARATOR_HEIGHT;
			}
			p.x += mg.getWidth(col);
			p.x += MergeGrid.SEPARATOR_WIDTH;
		}
		int x = 0;
		for (int col = 0; col < mg.getNumColumns(); col++) {
			x += mg.getWidth(col);
			mg.getColSplitter(col).setLocation(x, 0);
			x += MergeGrid.SEPARATOR_WIDTH;
			mg.setComponentZOrder(mg.getColSplitter(col), curZOrder--);
		}
		int y = 0;
		for (int row = 0; row < mg.getNumRows(); row++) {
			y += mg.getHeight(row);
			mg.getRowSplitter(row).setLocation(0, y);
			y += MergeGrid.SEPARATOR_HEIGHT;
			mg.setComponentZOrder(mg.getRowSplitter(row), curZOrder--);
		}

		MergeGridGlassPane g = mg.getGlassPane();

		if (curZOrder != 0)
			LoggerFactory.getLogger(MergeGridLayout.class.getName()).error("curZOrder not at zero, something wrong");

		mg.setComponentZOrder(g, 0);

		// when ctrl is down all interaction is with the glasspane
		g.setBounds(mg.getBounds());
	}

}
