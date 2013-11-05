/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

/**
 * @author Stu Baurmann
 */
public class OldGlanceAction {
	/*
	private static Logger	theLogger = Logger.getLogger("com.hansonrobotics.app.animation.GlanceAction");

	public enum GlanceState {
		// NO_SUBJECT,
		STARE_MAIN,
		MOVE_TO_OTHER,
		STARE_OTHER,
		RETURN_TO_MAIN
	}
	private		GlanceStrategy		myGlanceStrategy;
	private		GlanceState	myState = GlanceState.STARE_MAIN;
	
	private		Long		firstFaceLockNoticeMsec;
	private		Long		lastFaceLockNoticeMsec;
	private		Long		startedGlanceMoveMsec;
	
	private		GazeJointStateFrame			savedMainGJSF;
	
	private		Random		myRandom = new Random();
	
	private		Point		otherRelativeVec = null;

	private static double	FACE_THRUST_ANGLE_ERROR_MAX = Math.PI / 2.0;
	private		int			myEscapeFrames = 0;
	
	public OldGlanceAction(GlanceStrategy gs) {
		myGlanceStrategy = gs;
	}
	public GlanceState getState() {
		return myState;
	}

	public Point updateStateAndSelectFaceTargetPoint(Point center, List<Rectangle> faceRectList, 
					GazeJointStateFrame gjsf) {
		Point targetPoint = null;
		long nowMsec = TimeUtils.currentTimeMillis();
		boolean faceLocked = false;
		Point closestFacePoint = findClosestFacePoint(center, faceRectList);
		if (myState == GlanceState.STARE_MAIN) {
			boolean glanceEligible = updateStareMainTimersAndCheckGlanceEligible(center, 
						closestFacePoint);
			if (glanceEligible) {
				theLogger.info("*******************************************Glance eligible, switching to MOVE_OTHER");
				myState = GlanceState.MOVE_TO_OTHER;
				startedGlanceMoveMsec = nowMsec;
				// store servo snapshot of current position
				savedMainGJSF = gjsf.copy();
				// select "distant" target to glance at
				List<Point> distantFacePoints = findFacePointsOutsideRadius(faceRectList, 
						center, myGlanceStrategy.getGlanceTargetMinRadiusPix());
				int numDFP = distantFacePoints.size();
				if (numDFP > 0) {
					int facePointChoice = 0;
					if (numDFP > 1) {
						facePointChoice = myRandom.nextInt(numDFP);
					}
					targetPoint = distantFacePoints.get(facePointChoice);
				}
				if (targetPoint != null) {
					int relativeX = targetPoint.x - center.x;
					int relativeY = targetPoint.y - center.y;
					otherRelativeVec = new Point (relativeX, relativeY);
					theLogger.info("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Selected other face point at otherRelativeVec=" + otherRelativeVec );
				}
			} else {
				// theLogger.finer("*********************************NOT Glance eligible, returning closestFacePoint: " + closestFacePoint );
				targetPoint = closestFacePoint;
			}
		} else if (myState == GlanceState.STARE_OTHER || myState == GlanceState.MOVE_TO_OTHER) {
			long glanceDurationMsec = nowMsec - startedGlanceMoveMsec;
			if (glanceDurationMsec > myGlanceStrategy.getGlanceHoldMinSec() * 1000.0) {
				theLogger.info("*******************************************Glance expired, restoring snapshot");
				// Glance period has expired, need to restore the saved servo snapshot
				myState = GlanceState.RETURN_TO_MAIN;
				myEscapeFrames = 0;
				// No target set.
			} else {
				if  (myState == GlanceState.STARE_OTHER) {
					// This will be the right face IF we jumped far enough during the MOVE_TO_OTHER state.
					targetPoint = closestFacePoint;
				} else {
					// myState == GlanceState.MOVE_TO_OTHER) 
					targetPoint =  getOtherMoveTarget(faceRectList, center);
				}	
			}
		} else if (myState == GlanceState.RETURN_TO_MAIN) {
			// One frame of glancing should be enough.....err, but it's not.
			myEscapeFrames++;
			if (myEscapeFrames >= myGlanceStrategy.getEscapeThrustFrames()) {
				theLogger.info("*******************************************Snapshot restored, switching back to normal mode");
				myState = GlanceState.STARE_MAIN;
				clearTimers();
				targetPoint = closestFacePoint;
			} else {
				
			}
		}
		return targetPoint;
	}	
	private Point getOtherMoveTarget(List<Rectangle> faceRectList, Point center) {
		Point targetPoint = null;
		Point goalFacePoint = null;
		if (otherRelativeVec != null) {
			goalFacePoint = findFacePointOnVector(faceRectList, center, this.otherRelativeVec);
			if (goalFacePoint != null) {
				double goalFaceTheta = getPolarTheta(center, goalFacePoint);
				targetPoint = goalFacePoint;
				int relativeX = goalFacePoint.x - center.x;
				int relativeY = goalFacePoint.y - center.y;
				otherRelativeVec = new Point (relativeX, relativeY);				
				this.myEscapeFrames++;
				// Do this using distance - compared to distance from "main" face, which may have disappeared.
			}
		} else {
			// We are seeking motion, not a face.
			myEscapeFrames++;
		}
		if (myEscapeFrames >= myGlanceStrategy.getEscapeThrustFrames()) {
			myState = GlanceState.STARE_OTHER;
		}
		theLogger.finer("getOtherMoveTarget returning: " + targetPoint + " --  myEscapeFrames=" + myEscapeFrames );
		return targetPoint;
	}
	private void clearTimers() {
		firstFaceLockNoticeMsec = null;
		lastFaceLockNoticeMsec = null;
		startedGlanceMoveMsec = null;
		myEscapeFrames = 0;
		otherRelativeVec = null;
	}
	public boolean adjustTargetFrame(GazeJointStateFrame targetFrame) {
		if (myState == GlanceState.RETURN_TO_MAIN) {
			theLogger.fine("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  Adjusting target frame");
			targetFrame.setTargetsFromSnapFrameCurrents(savedMainGJSF);
			return true;
		} else {
			return false;
		}
	}
	private boolean  updateStareMainTimersAndCheckGlanceEligible(Point center, 
				Point closestFacePoint) {
		long nowMsec = TimeUtils.currentTimeMillis();
		boolean glanceEligible = false;
		boolean faceLocked = false;
		if (closestFacePoint != null) {
			double closestFaceDistance = closestFacePoint.distance(center);
			double faceLockRadius = myGlanceStrategy.getStareNoticeRadiusPix();
			if (closestFaceDistance <= faceLockRadius ) {
				// theLogger.fine("**************** found face within lock radius");
				faceLocked = true;
				lastFaceLockNoticeMsec = nowMsec;
				if (firstFaceLockNoticeMsec == null) {
					firstFaceLockNoticeMsec = nowMsec;
				}
			}
		}
		if (!faceLocked && (lastFaceLockNoticeMsec != null)) {
			// We're not currently locked, but recently we have been locked.  Check expiration.
			if ((nowMsec - lastFaceLockNoticeMsec) >  myGlanceStrategy.getStareResetDelaySec() * 1000.0) {
				// Face lock has expired, so reset the timers.
				theLogger.fine("*************************************** Face stare-lock expired");

				clearTimers();
			}
		}
		if ((firstFaceLockNoticeMsec != null) && (lastFaceLockNoticeMsec != null)) {
			long faceLockDurationMsec = lastFaceLockNoticeMsec - firstFaceLockNoticeMsec;
			if (faceLockDurationMsec >= myGlanceStrategy.getStareHoldMinSec() * 1000.0) {
				glanceEligible = true;
			}
		}
		return glanceEligible;
	}
	public static Point findClosestFacePoint(Point center, List<Rectangle> faceRectList) {
		Point targetPoint = null;
		Double targetDistance = null;		
		for (Rectangle faceRect : faceRectList) {
			Point facePoint = OldGazeAnimator.getRectangleCenterPoint(faceRect);
			double facePointDistance = facePoint.distance(center);
			if ((targetPoint == null) || (facePointDistance < targetDistance)) {
				targetPoint = facePoint;
				targetDistance = facePointDistance;
			}
		}
		return targetPoint;
	}
	private List<Point> findFacePointsOutsideRadius(List<Rectangle> faceRects, Point center, 
					double radius) {
		List<Point>		farFaces = new ArrayList<Point>();
		for (Rectangle r : faceRects) {
			Point facePoint = OldGazeAnimator.getRectangleCenterPoint(r);
			double distance = facePoint.distance(center);
			if (distance >= radius) {
				farFaces.add(facePoint);
			}
		}
		return farFaces;
	}
	// directionVec is relative to 0,0, NOT to "center" point.
	private Point findFacePointOnVector (List<Rectangle> faceRects, Point center, Point directionVec) {
		double vecTheta = getPolarTheta(directionVec);
		theLogger.finer("*******************matching thrust vector angle: " + vecTheta);
		Double bestDistance = null;
		Point bestFacePoint = null;
		for (Rectangle r : faceRects) {
			Point facePoint = OldGazeAnimator.getRectangleCenterPoint(r);
			theLogger.finer("Testing facePoint at: " + facePoint);
			double faceTheta = getPolarTheta(center, facePoint);		
			double thetaDistance = thetaDistance(faceTheta, vecTheta);
			if ((bestDistance == null) || (thetaDistance < bestDistance)) { 
				bestFacePoint = facePoint;
				bestDistance = thetaDistance;
			}
		}
		if (bestFacePoint != null) {
			if (bestDistance > FACE_THRUST_ANGLE_ERROR_MAX) {
				theLogger.warning("***************** Discarding thrust face at " + bestFacePoint + " with angle error " + bestDistance);
				// but, this allows motion target to take over...
				bestFacePoint = null;
			} else {
				theLogger.warning("***************** Continuing motion toward face at " + bestFacePoint + " with angle error " + bestDistance);
			}
		} else {
			theLogger.warning("***************** No face point found, looking in direction: " + directionVec);
		}
		return bestFacePoint;
	}
	// Compute version of angle that is closest to zero (between -pi and +pi)
	public static double normalizedAngle(double ang) {
		return SmallAngle.normalizedAngle(ang);
	}
	private static double thetaDistance(double theta1, double theta2) {
		double ndiff = normalizedAngle(theta1 - theta2);
		double mag = Math.abs(ndiff);
		return mag;
	}
	private static double getPolarTheta(Point center, Point vec) {
		int relX = vec.x - center.x;
		int relY = vec.y - center.y;
		return Math.atan2(relY, relX);
	}
	private static double getPolarTheta(Point vec) {
		return Math.atan2(vec.y, vec.x);
	}
	*/
}
