/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.gaze.estimate;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.List;



import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.Joint;
import org.cogchar.api.animoid.protocol.JointPosition;
import org.cogchar.api.animoid.protocol.SmallAngle;
import org.cogchar.api.animoid.world.SummableWorldJointList;

import org.cogchar.sight.api.core.SightPort;
import org.cogchar.sight.api.core.SightDirectionComputer;

import static org.cogchar.api.animoid.protocol.JointStateCoordinateType.*;
import org.friendularity.gaze.api.GazeJoint;


/**
 * Crude forward-kinematics computation to determine what direction a
 * visible rectangle's center is in.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class GazeDirectionComputer extends SightDirectionComputer {
	private static Logger	theLogger = LoggerFactory.getLogger(GazeDirectionComputer.class.getName());

	private	List<GazeJoint>		myGazeJoints;
	private SummableWorldJointList<GazeJoint>	myAzimuthSWJL, myElevationSWJL;
	
	public GazeDirectionComputer(SightPort vp, List<GazeJoint> gjList) {
		super(vp);
		theLogger.info("GDC constructed with viewport: " + vp);
		myGazeJoints = gjList;
		myAzimuthSWJL = new SummableWorldJointList<GazeJoint>();
		myElevationSWJL =  new SummableWorldJointList<GazeJoint>();
		for (GazeJoint gj : myGazeJoints) {
			if (gj.isHorizontal()) {
				myAzimuthSWJL.addWorldJoint(gj);
			} else {
				myElevationSWJL.addWorldJoint(gj);
			}
		}
	}
	// This "center" is not the center pixel, unless SightPort skew is 0.0.
	public EgocentricDirection computeGazeCenterDirection(Frame f) {
		double azimuthDeg = 0.0;
		double elevationDeg = 0.0;
		
		for (GazeJoint gj : myGazeJoints) {
			GazeJoint.Direction direction = gj.getPositiveDirection();
			Integer logicalJointID = gj.getLogicalJointID();
			Double romDegrees = gj.getRangeOfMotionDegrees();
			JointPosition rawJP = f.getJointPositionForOldLogicalJointNumber(logicalJointID);
			JointPosition absJP = rawJP.convertToCooordinateType(FLOAT_ABS_RANGE_OF_MOTION);
			double posAbsROM = absJP.getCoordinateFloat(FLOAT_ABS_RANGE_OF_MOTION);
			Joint j = rawJP.getJoint();
			JointPosition centerAJP = j.getCenterPosition();
			double centerAbsROM = centerAJP.getCoordinateFloat(FLOAT_ABS_RANGE_OF_MOTION);
			double posRelToCenter = posAbsROM - centerAbsROM;
			double degreeOffset = romDegrees * posRelToCenter;
			switch(direction) {
			case RIGHT:
				azimuthDeg += degreeOffset;
			break;
			case LEFT:
				azimuthDeg -= degreeOffset;
			break;
			case UP:
				elevationDeg += degreeOffset;
			break;
			case DOWN:
				elevationDeg -= degreeOffset;
			break;	
			}
		}
		
		SmallAngle az = SmallAngle.makeFromDeg(azimuthDeg);
		SmallAngle el = SmallAngle.makeFromDeg(elevationDeg);
		return new EgocentricDirection(az, el);
	}
	//
	public EgocentricDirection	computeGazeDirection(Frame f, Rectangle targetRect) {
		Point rectCenterPoint = getRectangleCenterPoint(targetRect);
		return computeGazeDirection(f, rectCenterPoint);
	}
	public EgocentricDirection	computeGazeDirection(Frame f, Point targetPoint) {
		EgocentricDirection edir = null;
		
		EgocentricDirection centerDir = computeGazeCenterDirection(f);

		// Add angle contributions for position of the face in the camera F.O.V. relative to center.
		
		SightPort sightPort = getViewPort();
		SmallAngle azFOV = sightPort.getAzimuthAngleForScreenHorizPixel(targetPoint.x);
		SmallAngle elFOV = sightPort.getElevationAngleForScreenVertPixel(targetPoint.y);
		SmallAngle azTotal = centerDir.getAzimuth().add(azFOV);
		SmallAngle elTotal = centerDir.getElevation().add(elFOV);
		edir = new EgocentricDirection(azTotal, elTotal);
		return edir;
	}

	public Point computeTargetScreenPoint(Frame jps, EgocentricDirection targetDir) {
		EgocentricDirection centerDir = computeGazeCenterDirection(jps);
		EgocentricDirection relativeDir = targetDir.subtract(centerDir);
		SightPort sightPort = getViewPort();
		int hpix = sightPort.getCameraHorizPixelForAzimuthAngle(relativeDir.getAzimuth());
		int vpix = sightPort.getCameraVertPixelForElevationAngle(relativeDir.getElevation());
		// hpix and vpix may well be "off screen", i.e. negative or "too big"
		Point	targetPoint = new Point(hpix, vpix);
		return targetPoint;
	}

	public SmallAngle getApproximateAngleForScreenPixelDiameter(double pixelDiam) {
		SightPort sightPort = getViewPort();
		// Quick and Dirty: Use a horizontal diameter
		double widthScreenFraction = pixelDiam / sightPort.getWidthPixels();
		SmallAngle viewPortWidthAngle = sightPort.getWidthAngle();
		double diameterRadians = viewPortWidthAngle.getRadians() * widthScreenFraction;
		SmallAngle widthAngle = SmallAngle.makeFromRad(diameterRadians);
		return widthAngle;
	}
	

	public EgocentricDirection computeGazeVelocity(Frame velFrame) {
		double azimuthVelDeg = 0.0;
		double elevationVelDeg = 0.0;

		for (GazeJoint gj : myGazeJoints) {
			GazeJoint.Direction direction = gj.getPositiveDirection();
			Integer logicalJointID = gj.getLogicalJointID();
			Double romDegrees = gj.getRangeOfMotionDegrees();
			JointPosition rawJV = velFrame.getJointPositionForOldLogicalJointNumber(logicalJointID);
			double velAbsROM = rawJV.getCoordinateFloat(FLOAT_VEL_RANGE_OF_MOTION_PER_SEC);
			Joint j = rawJV.getJoint();
			JointPosition centerAJP = j.getCenterPosition();
			double velDegrees = romDegrees * velAbsROM;
			switch(direction) {
			case RIGHT:
				azimuthVelDeg += velDegrees;
			break;
			case LEFT:
				azimuthVelDeg -= velDegrees;
			break;
			case UP:
				elevationVelDeg += velDegrees;
			break;
			case DOWN:
				elevationVelDeg -= velDegrees;
			break;
			}
		}
		SmallAngle az = SmallAngle.makeFromDeg(azimuthVelDeg);
		SmallAngle el = SmallAngle.makeFromDeg(elevationVelDeg);
		return new EgocentricDirection(az, el);
	}
	public SummableWorldJointList<GazeJoint> getAzimuthSummableJoints() {
		return myAzimuthSWJL;
	}
	public SummableWorldJointList<GazeJoint> getElevationSummableJoints() {
		return myElevationSWJL;
	}
	public double getMinViewableAzimuthDeg() {
		SmallAngle minCenterAz = myAzimuthSWJL.getMinTotalWorldPosAngle();
		return getViewPort().getMinViewableAzForCenter(minCenterAz).getDegrees();
	}
	public double getMaxViewableAzimuthDeg() {
		SmallAngle maxCenterAz = myAzimuthSWJL.getMaxTotalWorldPosAngle();
		return getViewPort().getMaxViewableAzForCenter(maxCenterAz).getDegrees();
	}
	public double getMinViewableElevationDeg() {
		SmallAngle minCenterEl = myElevationSWJL.getMinTotalWorldPosAngle();
		return getViewPort().getMinViewableElForCenter(minCenterEl).getDegrees();
	}
	public double getMaxViewableElevationDeg() {
		SmallAngle maxCenterEl = myElevationSWJL.getMaxTotalWorldPosAngle();
		return getViewPort().getMaxViewableElForCenter(maxCenterEl).getDegrees();
	}

}
