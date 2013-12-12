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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager2;
import java.awt.Point;

/**
 *
 * @author Annie
 */
public class HorizontalBentoLayoutManager implements LayoutManager2,
		java.io.Serializable {

	// alternative - hold on to the BentoBox
	private float dedicatedWeight = 0.0f;
	
    /**
     * Adds the specified component to the layout, using the specified
     * constraint object.  For HorizontalBentoLayoutManager the constraint
	 * must be the weight for this component
     * <p>
     * Most applications do not call this method directly. This method
     * is called when a component is added to a container using the
     * <code>Container.add</code> method with the same argument types.
     * @param   comp         the component to be added.
     * @param   constraints  a float that says where this object is to be added.
     * @see     java.awt.Container#add(java.awt.Component, java.lang.Object)
     * @exception   IllegalArgumentException  if the constraint object is not
     *                 a string, or if it not one of the five specified
	 *              constants.
     */
	@Override
	public void addLayoutComponent(Component comp, Object constraints) {
		synchronized (comp.getTreeLock()) {
			if (comp instanceof HorBentoSplitter) {
				;
			} else if (
					comp instanceof BentoPlugin && 
					constraints instanceof Float &&
					dedicatedWeight + ((Float)constraints).floatValue() < 1.0
					) {
				dedicatedWeight += ((Float)constraints).floatValue();
			} else if (
					comp instanceof BentoPlugin && 
					constraints instanceof Float
					) {
				throw new IllegalArgumentException("cannot add to layout: constraints must add to < 1.0");
			} else if (constraints instanceof Float) {
				throw new IllegalArgumentException("cannot add to layout: comp must be BentoPlugin or HorBentoSplitter");
			} else {
				throw new IllegalArgumentException("cannot add to layout: constraint must be a Float");
			}
		}
	}

	 /** 
     * Calculates the maximum size dimensions for the specified container,
     * given the components it contains.
     * @see java.awt.Component#getMaximumSize
     * @see LayoutManager
     */
	@Override
	public Dimension maximumLayoutSize(Container target) {
		Dimension d = new Dimension(0, 0);
		
		for(int i = 0 ; i < target.getComponentCount() ; i++)
		{
			Dimension max = target.getComponent(i).getMaximumSize();
			if (max.height > d.height)d.height = max.height;
			d.width += max.width;
		}
		return d;
	}

	/**
     * Returns the alignment along the x axis.  This specifies how
     * the component would like to be aligned relative to other 
     * components.  The value should be a number between 0 and 1
     * where 0 represents alignment along the origin, 1 is aligned
     * the furthest away from the origin, 0.5 is centered, etc.
     */
	@Override
	public float getLayoutAlignmentX(Container target) {
		return 0.0f;
	}
	
    /**
     * Returns the alignment along the y axis.  This specifies how
     * the component would like to be aligned relative to other 
     * components.  The value should be a number between 0 and 1
     * where 0 represents alignment along the origin, 1 is aligned
     * the furthest away from the origin, 0.5 is centered, etc.
     */
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
		;
	}

	@Override
	public void addLayoutComponent(String name, Component comp) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override
	public void removeLayoutComponent(Component comp) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override
	public Dimension preferredLayoutSize(Container parent) {
		return maximumLayoutSize(parent);
	}

	@Override
	public Dimension minimumLayoutSize(Container parent) {
		return maximumLayoutSize(parent);
	}

	// this is where we do the real work. We do some computation
	// and then call setbounds on each component in the parent
	// we don't actually dink with parent
	@Override
	public void layoutContainer(Container parent) {
		Point ulpos = new Point(0,0);
		
		for(int i = 0 ; i <parent.getComponentCount() ; i++)
		{
			Component c = parent.getComponent(i);
			
			Point loc = c.getLocation();
			
			c.setLocation(ulpos);
			ulpos.x += c.getWidth();
		}
	}
}
