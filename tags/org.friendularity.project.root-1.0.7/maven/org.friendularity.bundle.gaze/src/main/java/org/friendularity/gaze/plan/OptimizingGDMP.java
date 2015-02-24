/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gaze.plan;


/*
import org.apache.commons.math3.FunctionEvaluationException;
import org.apache.commons.math3.analysis.DifferentiableMultivariateRealFunction;
import org.apache.commons.math3.analysis.MultivariateRealFunction;
import org.apache.commons.math3.analysis.MultivariateVectorialFunction;
import org.apache.commons.math3.optimization.GoalType;
import org.apache.commons.math3.optimization.RealConvergenceChecker;
import org.apache.commons.math3.optimization.RealPointValuePair;
import org.apache.commons.math3.optimization.SimpleScalarValueChecker;
import org.apache.commons.math3.optimization.direct.DirectSearchOptimizer;
import org.apache.commons.math3.optimization.direct.MultiDirectional;
*/
import org.cogchar.animoid.calc.optimize.MultiStepPJT;
import org.cogchar.animoid.calc.optimize.ParameterVector;
import org.jscience.mathematics.vector.Float64Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class OptimizingGDMP extends BudgetedGDMP {
	private static Logger	theLogger = LoggerFactory.getLogger(OptimizingGDMP.class.getName());

	private		double[]			myLastOptimalParams;



	public OptimizingGDMP(Integer numIntervals, Double planStartSec, Double maxDurSec, Double frameDurSec) {
		super(numIntervals, planStartSec, maxDurSec, frameDurSec);
	}
/*
	public double[] computeOptimalParams(double goalDeg) throws Throwable {
		setGoalValue(goalDeg);
		// DirectSearchOptimizer	optimizer = new NelderMead();
		DirectSearchOptimizer	optimizer = new MultiDirectional();
		optimizer.setMaxIterations(20);
		optimizer.setMaxEvaluations(200);
		RealConvergenceChecker checker = new SimpleScalarValueChecker(1.0, -1.0);
		optimizer.setConvergenceChecker(checker);
		double[] initParams = myLastOptimalParams;
		if (initParams == null) {
			initParams = getFreshStartingParams(DUR_PER_JOINT);
		}
		MultivariateRealFunction  costFunction = getCostFunction(goalDeg);
		RealPointValuePair optimizedPair = optimizer.optimize(costFunction, GoalType.MINIMIZE, initParams);
		double cost = optimizedPair.getValue();
		double[] optimalParams = optimizedPair.getPoint();
		theLogger.trace("Optimized gaze plan cost is: " + cost
				+ ", numIterations=" + optimizer.getIterations()
				+ ", numEvaluations=" + optimizer.getEvaluations()
				+ ", rawParamVec=" + ParameterVector.doubleArrayToString(optimalParams));
;
		myLastOptimalParams = optimalParams;
		return optimalParams;
	}
	* */
	public void optimize(double goalDeg) throws Throwable {
		throw new Exception("Needs update to Apache Commons Math 3.1.1");/*
		 * 
		 
		double optimalParams[] = computeOptimalParams(goalDeg);
		reconfigureUsingParams(optimalParams, 0);
		* 
		*/ 
	}
/*
	public MultivariateRealFunction getCostFunction(final double targetPosDeg) {
		return new MultivariateRealFunction() {
			public double value(double[] args) throws FunctionEvaluationException, IllegalArgumentException {
				theLogger.trace("RawParams: " + ParameterVector.doubleArrayToString(args));
				reconfigureUsingParams(args, 0);
				// theLogger.fine("After reconfig, plan=" + GazeDimensionMotionPlan.this.toString());
				double totalCost = computePlanTotalCost(targetPosDeg, DUR_SHARED, true);
				return totalCost;
				// Unit Test:   return ParameterVector.sumSquaredDoubleArray(args);
			}
		};
	}
	public DifferentiableMultivariateRealFunction getDifferentiableCostFunction(final double targetPosDeg) {
		return new DifferentiableMultivariateRealFunction() {
			public double value(double[] args) throws FunctionEvaluationException, IllegalArgumentException {
				theLogger.trace("RawParams: " + ParameterVector.doubleArrayToString(args));
				reconfigureUsingParams(args, 0);
				// theLogger.fine("After reconfig, plan=" + GazeDimensionMotionPlan.this.toString());
				double totalCost = computePlanTotalCost(targetPosDeg, DUR_SHARED, true);
				return totalCost;
				// Unit Test:   return ParameterVector.sumSquaredDoubleArray(args);
			}

			public MultivariateVectorialFunction gradient() {
				return new MultivariateVectorialFunction() {
					public double[] value(double[] arg0) throws FunctionEvaluationException, IllegalArgumentException {
						return null;
					}
				};
			}

			public MultivariateRealFunction partialDerivative(int arg0) {
				return new MultivariateRealFunction() {
					public double value(double[] arg0) throws FunctionEvaluationException, IllegalArgumentException {
						return 0.0;
					}
				};
			}
		};
	}
	private double [] getFreshStartingParams(boolean abbrevDurationsPerJoint) {

		int paramCntTot = getParamCountTotal(abbrevDurationsPerJoint);
		double [] params = new double[paramCntTot];
		// theLogger.fine("Initializing " + paramCntTot + " total parameters for " + myJointPlans.size()
		//		+ " jointPlans with " + myIntervalsPerJoint + " planIntervals each.");
		int cursor = 0;
		int parametricDurCount = getIndependentlyElasticDurationCount();
		if (!abbrevDurationsPerJoint) {
			resetDurationsVecToNominal(mySharedDurationPV);
			// Shared durations are first thing in param vec.
			mySharedDurationPV.writeValuesToArray(params, cursor, 1, parametricDurCount);
			cursor += parametricDurCount;
		}
		for (GazeJointMotionPlan gjmp: myJointPlans) {
			MultiStepPJT mspjt = gjmp.getMultiStepPJT();
			resetAccelLevelVecToNominal(mspjt.getLevelPV());
			if (abbrevDurationsPerJoint) {
				ParameterVector durationPV = mspjt.getDurationPV();
				resetDurationsVecToNominal(durationPV);
			}
			cursor += mspjt.writeToArray(params, cursor, abbrevDurationsPerJoint);
		}
		if (cursor != paramCntTot) {
			throw new RuntimeException("Ouch! Param Init programming error");
		}
		return params;
	}
*/

	/**
	 * Compute partial derivatives w.r.t. acceleration and duration parameters,
	 * evaluated at the "current" configured parameter values.
	 *
	 * Each acceleration affects the position+error contributions of it's own curve
	 * and all subsequent curves in the same sequence, through indirect effects on
	 * their initial positions and velocities.
	 *
	 * Each duration affects the position+error contributions of all curves to which
	 * it is applied (i.e. all curves in the segment), and all curves following those
	 * curve in sequence (i.e. all curves in all following segments), again through
	 * indirect effects on their initial positions and velocities.
	 *
	 * We use the chain rule to decompose the impact of each accel+dur parameter
	 * on the position+error contributions, via the derivatives of
	 */
	public  Float64Vector computeTargetDistanceCostGradient(double targetPosDeg) {
		Float64Vector accelGrad = computeTDC_AccelGradient();
		return null;
	}
	public Float64Vector computeTDC_AccelGradient() {
		return null;
	}
	public Float64Vector computeTDC_DurationGradient() {
		return null;
	}

	/*
	 * Compute partial derivatives w.r.t. acceleration and duration parameters,
	 * evaluated at the "current" configured parameter values.
	 *
	 * The plan execution cost is generally a sum of time-integrals of functions of
	 * acceleration, velocity, and position, which are often composed of terms
	 * in the form "squared distance from a nominal value", where the nominal
	 * value may be 0 for accel and velocity, and "center" for position.
	 * The actual values of velocity and position depend on the set of all
	 * previous durations and accelerations. 
	 * 
	 * The position contribution is uniquely important and also complex.
	 * Our positions are represented using "offset degrees from joint center",
	 * but we do not generally have an equal amount of physically available
	 * angle on either side of "center".  This set of facts leads us to prefer
	 * an asymmetric cost contribution from the position
	 * 
	 */
	public Float64Vector computePlanExecutionCostGradient() {
		return null;
	}


	public String toString() {
		return "OptimizingGDMP[durations=" + mySharedDurationPV
			+ "\njointPlans=" + myJointPlans + "]";
	}
}
