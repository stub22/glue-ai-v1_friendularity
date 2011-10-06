/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.vision;


import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.geom.Point2D;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.vision.IAnnotatingObserver;

/**
 * @author Stu Baurmann
 */
public class GazeCalculationAnnotater implements IAnnotatingObserver {
	private static Logger	theLogger = Logger.getLogger(GazeCalculationAnnotater.class.getName());

	private		Point		myTargetPoint = null;
	private		int			myTargetRadius = 10;
	private		Point		myCenterPoint = null;
	private		Point2D		myDeltaPixVec = null;
	private		Color		myColor = null;
	
	public synchronized void setDrawColor(Color c) {
		myColor = c;
	}
	public synchronized void setTargetPoint(Point p) {
		myTargetPoint = p;
	}
	public synchronized void setTargetRadius(int radius) {
		myTargetRadius = radius;
	}
	public synchronized void setCenterPoint (Point p) {
		myCenterPoint = p;
	}
	public synchronized void setDeltaPixVec(Point2D dpv) {
		myDeltaPixVec = dpv;
	}
    public synchronized void Annotate(Graphics g) {
		try {
			if (myTargetPoint != null) {
				if (myColor == null) {
					myColor = Color.CYAN;
				}
				g.setColor(myColor);
				int ovalX = myTargetPoint.x - myTargetRadius;
				int ovalY = myTargetPoint.y - myTargetRadius;
				int ovalWidth = 2 * myTargetRadius;
				int ovalHeight = 2 * myTargetRadius;
				g.drawOval(ovalX, ovalY, ovalWidth, ovalHeight);
				// Draw a + in the middle of the circle.
				g.drawLine(myTargetPoint.x, ovalY, myTargetPoint.x, ovalY + 2 * myTargetRadius);
				g.drawLine(ovalX, myTargetPoint.y, ovalX + 2 * myTargetRadius, myTargetPoint.y);	
				if ((myCenterPoint != null) && (myDeltaPixVec != null)) {
					g.drawLine(myCenterPoint.x,	myCenterPoint.y, 
								(int) Math.round(myCenterPoint.x + myDeltaPixVec.getX()), 
								(int) Math.round(myCenterPoint.y + myDeltaPixVec.getY()));
				}
			}
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "myTargetPoint=" + myTargetPoint + ", g=" + g, t);
		}
	}
}
