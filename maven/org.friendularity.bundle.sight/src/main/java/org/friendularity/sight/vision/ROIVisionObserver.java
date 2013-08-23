/*
 * ROIVisionObserver.java
 * 
 * Created on Jul 24, 2007, 2:39:43 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import org.cogchar.zzz.oldboot.ThreadAwareObject;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author josh
 */
public class ROIVisionObserver extends ThreadAwareObject implements IAnnotatingObserver, IROIObserver {
	// Color to highlight ROIs
    protected int[] m_rects;
	protected int  myRectCount = 0;
    protected Color m_c;
	
    public ROIVisionObserver(Color c) {
		super();
        m_c = c;
    }
	protected List<Rectangle> getAnnotationRectangles() {
		return getRectanglesOfInterest();
	}
	
	public List<Rectangle> getRectanglesOfInterest() {
		List<Rectangle> rectList = new ArrayList<Rectangle>();
		for (int i =0; i < myRectCount; i++) {
			int	idx = 4 * i;
			Rectangle r = new Rectangle(m_rects[idx], m_rects[idx+1], m_rects[idx+2], m_rects[idx+3]);
			rectList.add(r);
		}
		return rectList;
	}
    public void Annotate(Graphics g) {
        g.setColor(m_c);
		List<Rectangle> rectList = getAnnotationRectangles();
		if (rectList != null) {
			for (Rectangle r : rectList) {
				g.drawRect(r.x, r.y, r.width, r.height);
				// m_rects[index], m_rects[index+1], m_rects[index+2], m_rects[index+3]);
			}
		}
    }
    public void ProcessFrame(int[] rectData) {
        m_rects = rectData;
		myRectCount = rectData.length / 4;
    }
    
}
