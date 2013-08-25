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

package org.friendularity.math.api;

import org.matheclipse.core.eval.SystemNamespace;
import org.matheclipse.core.interfaces.IAST;
import org.matheclipse.core.interfaces.IExpr;
import org.matheclipse.core.interfaces.ISymbol;
import org.matheclipse.core.interfaces.INumber;
import org.matheclipse.core.interfaces.INum;

import org.appdapter.core.log.BasicDebugger;
/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class MathGate extends BasicDebugger {

	abstract  public IExpr parseAndEvalExprToIExpr(String expr);
	abstract public void putVar(String name, Object var);
	
	private static void registerFuncPackage(String pkgName) {
		SystemNamespace.DEFAULT.add(pkgName);
	}
	
	public IAST parseAndEvalExprToIAST(String expr) {
		IAST result = null;
		IExpr expResult = parseAndEvalExprToIExpr(expr);
		if (expResult != null) {
			if (expResult instanceof IAST) {
				result = (IAST) expResult;
			} else {
				logWarning("Expected IAST, but got " + expResult.getClass());
			}
		}
		return result;
	} 
	public double[] parseAndEvalExprToDoubleVec(String expr, double[] optStorageToUpdate) {
		IAST treeResult = parseAndEvalExprToIAST(expr);
		return writeTreeResultIntoArray(treeResult, optStorageToUpdate);
	}
	public double[] writeTreeResultIntoArray(IAST treeResult, double[] optStorageToUpdate) {
		double[] result = (optStorageToUpdate != null) ? optStorageToUpdate:  new double[0];

		if (treeResult != null) {
			int treeEvalFlags = treeResult.getEvalFlags();
			ISymbol treeTypeSymbol = treeResult.topHead();
			int  typeSymAttribs = treeTypeSymbol.getAttributes();
			int iastSize = treeResult.size();
			// logInfo("treeSize=" + iastSize + ", treeTypeSymbol= " + treeTypeSymbol + ", attribs=" + Integer.toHexString(typeSymAttribs) + 
			// 			", evalFlags=" + Integer.toHexString(treeEvalFlags) + ", treeResult=" + treeResult);
			if (treeResult.isList()) {
				if (iastSize > 1) {
					// Start by assuming all the vals are always double-convertible.

					int valCount = iastSize - 1;
					int tgtSize = result.length;
					if (valCount !=  tgtSize) { 
						if (tgtSize == 0) {
							tgtSize = valCount;
							result = new double[tgtSize];
						} else {
							getLogger().warn("Math result vector size {} does not match tgt storage size {}", valCount, tgtSize);
						}
					} else {
						// getLogger().debug("Hooray, the array sizes match!");
					}
					for (int resIdx = 0; (resIdx < valCount) && (resIdx < tgtSize); resIdx++) {
						INumber resultNumberAny = treeResult.getNumber(resIdx + 1);
						INum  resultNumDouble = (INum) resultNumberAny;
						result[resIdx] = resultNumDouble.getRealPart();
					}
					// Another way:
					// Special iterator() skips over function symbol element #0
					// for (IExpr vExp : treeResult) {
					//	logInfo("Got vExp " + vExp + " of type " + vExp.getClass());
					//}
					
				}
			} else {
				logWarning("TreeResult is not a list: " + treeResult);
			}
		}

		return result; 
	}


}
