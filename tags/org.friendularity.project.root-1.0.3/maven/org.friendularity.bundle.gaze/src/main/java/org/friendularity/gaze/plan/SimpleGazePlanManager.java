/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gaze.plan;

import org.friendularity.gaze.api.GazeDimension;
import java.util.HashMap;
import java.util.Map;

import org.appdapter.bind.math.jscience.number.NumberFactory;
import org.jscience.mathematics.number.Number;
import org.jscience.mathematics.structure.Field;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class SimpleGazePlanManager<RN extends Number<RN> & Field<RN>> {
	private static Logger	theLogger = LoggerFactory.getLogger(SimpleGazePlanManager.class.getName());

	private		NumberFactory<RN>		myNumberFactory;
	private		Double					myFrameLengthSec;
	private		Map<String, PlanPair>	myPlansByStrategyName = new HashMap<String, PlanPair>();
	public class PlanPair {
		SimpleGazeDimensionPlan<RN>		myHorizontalPlan, myVerticalPlan;
		public PlanPair() {
			myHorizontalPlan = new SimpleGazeDimensionPlan<RN>(GazeDimension.HORIZONTAL, myNumberFactory, myFrameLengthSec);
			myVerticalPlan = new SimpleGazeDimensionPlan<RN>(GazeDimension.VERTICAL, myNumberFactory, myFrameLengthSec);
		}
		public SimpleGazeDimensionPlan<RN> getPlanForDimension(GazeDimension gd) {
			switch(gd) {
			case HORIZONTAL:
				return myHorizontalPlan;
			case VERTICAL:
				return myVerticalPlan;
			default:
				return null;
			}
		}
	}

	public SimpleGazePlanManager(NumberFactory<RN> numberFactory, double frameLengthSec) {
		myNumberFactory = numberFactory;
		myFrameLengthSec = frameLengthSec;
	}
	public SimpleGazeDimensionPlan<RN> getPlanForStrategyAndDimension(String strategyName,
				GazeDimension gd) {
		PlanPair pp = myPlansByStrategyName.get(strategyName);
		if (pp == null) {
			pp = new PlanPair();
			myPlansByStrategyName.put(strategyName, pp);
			theLogger.info("Constructed gaze plan for strategyName=" + strategyName + " and dim=" + gd);
		}
		return pp.getPlanForDimension(gd);
	}
}
