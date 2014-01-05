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
package org.friendularity.gmteach.symcalc;

import org.matheclipse.core.eval.EvalEngine;
import org.matheclipse.core.eval.EvalUtilities;
import org.matheclipse.core.expression.F;
import org.matheclipse.core.interfaces.IExpr;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class MathMemoryTest {
/*
 * 	 * @param fileName
	 *          <code>null</code> or optional text filename, which includes the
	 *          preloaded system rules
	 * @param symbolObserver
	 *          the observer for newly created <code>ISymbols</code>
	 * @param noPackageLoading
	 *          don't load any package at start up
	 */

	public static void main(final String args[]) {
		// MathSpaceFactory msf = new MathSpaceFactory();
		
		F.initSymbols(null, null, false);
		// Config.SERVER_MODE = true;
		
		EvalEngine	 evalEngine  = new EvalEngine();
		EvalUtilities evalUtilityWrapper = new EvalUtilities(evalEngine, false);
		String	vecExprText = "{-4.0, 3.0, -2.0, 1.0}";
		// int vecLen = 4;
		// double resultArr[] = new double[vecLen];	
		
		int ONE_MILLION = 1000 * 1000;
		
		float  mbFloat = 1024.0f*1024.0f;
		Runtime runtime = Runtime.getRuntime();
		
		long progStartStamp = System.currentTimeMillis();
		long lastStamp = progStartStamp;
		for (int outerIndex = 1; outerIndex < 100; outerIndex++) {
			evalUtilityWrapper.startRequest();
			String multiplyExprText = "" + outerIndex + " * " + vecExprText;
			IExpr parsedMultiplyExpr = evalEngine.parse(multiplyExprText);
			System.out.println("Loop # " + outerIndex + " parsed : " + parsedMultiplyExpr);
			IExpr resultExpr = null;
			for (int innerIndex = 0; innerIndex < ONE_MILLION; innerIndex++) {
				resultExpr = evalUtilityWrapper.evaluate(parsedMultiplyExpr);
			}
			System.out.println("Loop # " + outerIndex + " produced result IExpr on last step: " + resultExpr);
			System.gc();
			System.out.println("Free/Total/Max Memory in MB:  Free=" + runtime.freeMemory() / mbFloat 
							+ ", Total=" + runtime.totalMemory() / mbFloat + ", Max=" + runtime.maxMemory() / mbFloat);			
			long nowStamp = System.currentTimeMillis();
			double loopElapsedSec = (nowStamp - lastStamp) / 1000.0;			
			double totalElapsedSec = (nowStamp - progStartStamp) / 1000.0;
			lastStamp = nowStamp;
			System.out.println("Elapsed seconds loop= " + loopElapsedSec + ", total=" + totalElapsedSec );
			System.out.println("========================================================================");
		}
	}
}

