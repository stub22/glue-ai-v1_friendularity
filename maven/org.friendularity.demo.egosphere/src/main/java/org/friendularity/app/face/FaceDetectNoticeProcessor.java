/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import org.friendularity.app.jmxwrap.SignalStation;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.List;
import java.util.logging.Logger;
import org.cogchar.vision.ROIVisionObserver;


/**
 * @author Stu Baurmann
 */
public class FaceDetectNoticeProcessor extends ROIVisionObserver {
    private static Logger	theLogger = Logger.getLogger(FaceDetectNoticeProcessor.class.getName());
	
	private	FaceModel		myFaceModel = null;
	public SignalStation	mySignalStation = null;
	private		List<Rectangle> myLastRectangles;

	@Override protected List<Rectangle> getAnnotationRectangles() {
		return myLastRectangles;
	}
	public FaceDetectNoticeProcessor(Color c) {
		super(c);
		mySignalStation = SignalStation.getSignalStation();
	}
	public void ProcessFrame(int[] rectData) {
		this.blessCurrentThread();
		// superclass method records the rectData for use in drawing highlight boxes, etc.
		super.ProcessFrame(rectData);
		List<Rectangle> faceRects = getRectanglesOfInterest();
		if (myRectCount > 0) {
			// theLogger.fine("********************* FDNP notified of " + myRectCount + " faces in frame");
			if (myFaceModel != null) {				
				myFaceModel.facesSeen(faceRects);
			}
			/*
			if (mySignalStation != null) {
				mySignalStation.faceSeen();
			}
			*/
		}
		// We update the rectangles used for annotation only AFTER doing our updates
		// to the hypos.
		myLastRectangles = faceRects;
    }
	public void setFaceModel(FaceModel fm) {
		myFaceModel = fm;
	}
    public void Annotate(Graphics g) {
		super.Annotate(g);
		/*
		List<Rectangle> faceRects = getRectanglesOfInterest();
		for (Rectangle r : faceRects) {
			int xpos = r.x + 5;
			int ypos = r.y + 20;
			// g.drawString("yowza\nfocsle", xpos, ypos);
		}
		*/
	}
}
