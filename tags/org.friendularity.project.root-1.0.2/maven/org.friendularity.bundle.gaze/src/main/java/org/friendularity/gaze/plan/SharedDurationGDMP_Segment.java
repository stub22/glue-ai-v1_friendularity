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


import org.cogchar.animoid.calc.curvematrix.SDCACM_Interval;
import org.appdapter.bind.math.jscience.number.RealFuncs;
import org.jscience.mathematics.function.Polynomial;
import org.jscience.mathematics.number.Real;
import org.jscience.mathematics.vector.Vector;

public class SharedDurationGDMP_Segment {

	SDCACM_Interval myInterval;
	Polynomial<Real> myInstSignedErrPoly;
	Polynomial<Real> myInstSquaredErrPoly;
	// == instTargetErrorCost
	Polynomial<Real> myInstExecCostPoly;
	Polynomial<Real> myInstTotalCostPoly;
	Polynomial<Real> myIntegTotalCostPoly;
	Polynomial<Real> myIntegSqErrPoly;

	public SharedDurationGDMP_Segment(SDCACM_Interval interval, Polynomial<Real> targetPosPoly, SharedDurationGDMP outer) {
		super();
		myInterval = interval;
		int intervalNum = myInterval.myIntervalNum;
		myInstSignedErrPoly = myInterval.myPositionSumPoly.minus(targetPosPoly);
		myInstSquaredErrPoly = myInstSignedErrPoly.times(myInstSignedErrPoly);
		// separate target error integral is now for debug only, total cost is what matters
		myIntegSqErrPoly = myInstSquaredErrPoly.integrate(myInterval.myTimeOffsetVar);
		myInstExecCostPoly = outer.getInstantExecCostPolyForInterval(intervalNum);
		myInstTotalCostPoly = myInstSquaredErrPoly.plus(myInstExecCostPoly);
		myIntegTotalCostPoly = myInstTotalCostPoly.integrate(myInterval.myTimeOffsetVar);
	}

	public double getIntegTotalCostForTimeRange(double startOffset, double endOffset) {
		RealFuncs.setVariableValue(myInterval.myTimeOffsetVar, startOffset);
		double startValue = RealFuncs.evalPoly(myIntegTotalCostPoly);
		RealFuncs.setVariableValue(myInterval.myTimeOffsetVar, endOffset);
		double endValue = RealFuncs.evalPoly(myIntegTotalCostPoly);
		// Evaluate definite integral
		double result = endValue - startValue;
		return result;
	}

	public Vector<Real> getIntegTotalCostParamGradientForTimeRange(double startOffset, double endOffset) {
		RealFuncs.setVariableValue(myInterval.myTimeOffsetVar, startOffset);
		double startValue = RealFuncs.evalPoly(myIntegTotalCostPoly);
		RealFuncs.setVariableValue(myInterval.myTimeOffsetVar, endOffset);
		double endValue = RealFuncs.evalPoly(myIntegTotalCostPoly);
		// Evaluate definite integral
		double result = endValue - startValue;
		return null;
	}
	/**
	 * @return
	 * Gradient vector w.r.t. all accels and durations.
	 *
	 * For n intervals and m joints,  vector is of length n * (1 + m).
	 * Vector contains n duration partials first, then n * m accels, with contiguous
	 * accel blocks for each joint.
	 *
	 * To compute this vector, we compute two constituent jacobians and then apply
	 * the chain rule using matrix multiplication.  Our intermediate basis is the
	 * set of instantaneous state variables of all curves, which is of dimension
	 * 4 * n * m.
	 *
	 * The gradient of the IntegTotalCost over this intermediate basis is given
	 * by evaluating the polynomial derivatives of the IntegTotalCost polynomial.
	 * This evaluation must be done at a particular timeOffset within the segment,
	 * using the timeVariable shared by all the polynomials in this segment.
	 *
	 * The jacobian of the state variables over the parameters must be computed in
	 * a manner closely related to the state propagation in the
	 * CACM propagateEndpointConditions methods.
	 *
	 * Note that this latter jacobian, for each stateFrame=jointSegmentCurve, depends
	 * only on the prev/current parameters (accel + dur) for that joint, not on any
	 * other joints, and not on the "curve evaluation time" (which is independent of
	 * the interval config state whose  jacobian we are computing).  Specifically,
	 * the initPos and initVel state of a curve depends on all "final" state variables
	 * of previous curves, and all durations of previous curves, on the same joint.
	 * ("Final" merely refers to the fact that timeOffset=duration when we are
	 * propagating state impacts).
	 * 
	 * This dependence is translated into dependence on the 2 free parameters of
	 * each of those previous curves:  accel and duration.  Note that the intialPos
	 * and initalVel of the FIRST curve both have zero jacobian w.r.t. the accel+dur
	 * parameters.
	 *
	 */
	public Vector<Real> getIntegTotalCostParamGradientAtTimeOffset(Real timeOffset) {
		myInterval.myTimeOffsetVar.set(timeOffset);
		return null;
	}

	@Deprecated public double getIntegSquaredErrorForTimeRange(double startOffset, double endOffset) {
		RealFuncs.setVariableValue(myInterval.myTimeOffsetVar, startOffset);
		double startValue = RealFuncs.evalPoly(myIntegSqErrPoly);
		RealFuncs.setVariableValue(myInterval.myTimeOffsetVar, endOffset);
		double endValue = RealFuncs.evalPoly(myIntegSqErrPoly);
		// Evaluate definite integral
		double result = endValue - startValue;
		return result;
	}

}
