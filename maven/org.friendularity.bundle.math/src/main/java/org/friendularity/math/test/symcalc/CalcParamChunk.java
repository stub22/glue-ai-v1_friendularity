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
package org.friendularity.math.test.symcalc;

import org.cogchar.bind.symja.MathGateUnscripted;
import org.matheclipse.core.interfaces.IAST;
import org.matheclipse.core.interfaces.IExpr;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class CalcParamChunk extends ParamChunk {

	Logger theLogger = LoggerFactory.getLogger(CalcParamChunk.class);
	
	// 5 stages in evaluating a Symja expression (usually resulting in a vector-of-doubles output)
	
	// Input expression to be parsed.
	public String inExpr_01;
	
	// Parsed expression, not yet evaluated/simplified
	public IExpr prExpr_01;
	
	// Result of evaluating the parsed expression.  If it's a vector or matrix, or unresolved expr, it will be an IAST.
	// But if it's just a single number, e.g. an IntegerSym, then it is not an IAST.
	public IExpr outExpr_01;
	
	// Size of the output vector we expect.  This is a tricky issue, when the computations are high-volume.  
	public int resultSize_01;
	
	// Output vector resulting from evaluating the single expression.  
	// Various assumptions about the expression are involved in producing this vector.
	public double[] outNums_01;
	
	// Second pipeline, using separate instance variables, for exposure via Whackamole editor.
	public String inExpr_02;
	public IExpr prExpr_02;
	public IExpr outExpr_02;
	public int resultSize_02;
	public double[] outNums_02;
	
	public String inExpr_03;
	public IExpr prExpr_03;
	public IExpr outExpr_03;
	public int resultSize_03;
	public double[] outNums_03;

	public void doCalcs(MathGateUnscripted mg) {
		prExpr_01 = mg.parseCachableExpr(inExpr_01);
		outExpr_01 = mg.evalParsedExpr(prExpr_01, true, true);
		if (resultSize_01 > 0) {
			if (outExpr_01 instanceof IAST) {
				IAST outTree_01 = (IAST) outExpr_01;
				outNums_01 = mg.writeTreeResultIntoArray(outTree_01, null);
			} else {
				theLogger.warn("Result size is {}, but outExpr is not of type IAST: {}", resultSize_01, outExpr_01);
			}
		}
		// But now we would have to repeat the above code for blocks _02, _03, ...
		// or else refactor using reflection.
	}
}
