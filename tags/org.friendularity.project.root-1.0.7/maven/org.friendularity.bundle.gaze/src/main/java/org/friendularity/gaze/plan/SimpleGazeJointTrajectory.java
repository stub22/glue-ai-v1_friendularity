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

package org.friendularity.gaze.plan;


import org.cogchar.animoid.calc.curvematrix.RampingFramedCurveSeq;
import org.friendularity.gaze.estimate.GazeJointStateSnap;

import org.cogchar.api.animoid.world.WorldJoint;
import org.cogchar.api.animoid.world.WorldJointStateSnap;
import org.cogchar.api.animoid.world.WorldJointTrajectory;
import org.appdapter.bind.math.jscience.function.BumpUF;
import org.appdapter.bind.math.jscience.number.NumberFactory;
import org.friendularity.gaze.api.GazeJoint;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.jscience.mathematics.structure.Field;
import org.jscience.mathematics.number.Number;


class SimpleGazeJointTrajectory<RN extends Number<RN> & Field<RN>> extends WorldJointTrajectory<RN, RN> {

	private RampingFramedCurveSeq  mySeq;
	private GazeJointStateSnap myInitialJointStateSnap;
	private double myTargetDegreeYield;
	private double myFrameLengthSec;

	public SimpleGazeJointTrajectory(GazeJoint gj, RampingFramedCurveSeq seq,  double frameLengthSec,
				NumberFactory<RN> numFactory) {
		super(gj, 1.0, numFactory, null);
		mySeq = seq;
		myFrameLengthSec = frameLengthSec;		
	}

	public void setInititalJointStateSnap(GazeJointStateSnap gjss) {
		myInitialJointStateSnap = gjss;
	}

	RampingFramedCurveSeq getCurveSeq() {
		return (RampingFramedCurveSeq) mySeq;
	}

	public void setCurveSeqParams(double goalDeltaDeg) {
		RampingFramedCurveSeq cseq = getCurveSeq();
		GazeJointStateSnap gjss = myInitialJointStateSnap;
		GazeJointStrategy gsj = gjss.getGazeJointStrategy();
		WorldJoint wj = myWorldJoint;
		cseq.maxAccelMagPerFrame = gsj.rampAccelMaxDPSPS * myFrameLengthSec;
		cseq.maxDecelMagPerFrame = gsj.rampDecelMaxDPSPS * myFrameLengthSec;
		cseq.maxVelMagDPS = gsj.rampVelMaxDPS;
		cseq.initWorldPosDeg = gjss.getWorldPosDeg();
		cseq.initWorldVelDPS = gjss.getWorldVelDegPerSec();
		cseq.maxWorldPosDeg = wj.getWorldMaxDegreesOffset();
		cseq.minWorldPosDeg = wj.getWorldMinDegreesOffset();
		cseq.frameLenSec = myFrameLengthSec;
		cseq.goalDirectionSign = Math.signum(goalDeltaDeg);
	}

	public void calcNaiveParamsForMaximumYield(int frameCount) {
		RampingFramedCurveSeq cseq = getCurveSeq();
		cseq.syncInitialConditions();
		cseq.establishParamsForMaxYieldIgnoringPosConstraints(frameCount);
	}

	public double actualYieldForCurrentParamsUsingBoundaries(String dbgHeader, double truncWarnThresh, double negYieldWarnThresh) {
		return getCurveSeq().getYieldForCurrentParamsWithPosConstraints(dbgHeader, truncWarnThresh, negYieldWarnThresh).doubleValue();
	}

	public void setTargetDegreeYield(double tdy) {
		// TODO:  We need to follow this up with a computation to adjust the actual
		// accelerations.  This will correct the built-in overshoot of the
		// rampy gaze.
		myTargetDegreeYield = tdy;
	}

	@Override protected BumpUF getBumpFunction() {
		return mySeq.getBumpFunction();
	}

	@Override public double getFrameLengthSec() {
		return myFrameLengthSec;
	}

	@Override public WorldJointStateSnap getInitialJointStateSnap() {
		return myInitialJointStateSnap;
	}
}
