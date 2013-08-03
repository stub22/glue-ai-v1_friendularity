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

package org.friendularity.api.west;
import com.jme3.math.Vector3f;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import org.matheclipse.script.engine.MathScriptEngine;
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

public class MathGate extends BasicDebugger {
	ScriptEngine	myMathEng;
	
	public MathGate(ScriptEngine mathScriptEngine) {
		myMathEng = mathScriptEngine;
		enableTreeResults();
	}
	public void enableTreeResults() { 
		ScriptContext context = myMathEng.getContext();
		// If we do this, an AST is returned.  Otherwise a String is returned.
		context.setAttribute(MathScriptEngine.RETURN_OBJECT, Boolean.TRUE,	ScriptContext.ENGINE_SCOPE);
	}
	public IExpr evalToIExpr(String expr) {
		IExpr result = null;
		try {
			result = (IExpr) myMathEng.eval(expr);
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return result;
	}
	public IAST evalToIAST(String expr) {
		IAST result = null;
		IExpr expResult = evalToIExpr(expr);
		if (expResult != null) {
			if (expResult instanceof IAST) {
				result = (IAST) expResult;
			} else {
				logWarning("Expected IAST, but got " + expResult.getClass());
			}
		}
		return result;
	}
	// 2013-08-01 - looks like up until now, our impl here has been wasteful in terms of
	// allocating heap space.  
	public double[] readDoubleVec(String expr, double[] optStorageToUpdate) {
		double[] result = (optStorageToUpdate != null) ? optStorageToUpdate:  new double[0];

		IAST treeResult = evalToIAST(expr);
	
		if (treeResult != null) {
			int treeEvalFlags = treeResult.getEvalFlags();
			ISymbol treeTypeSymbol = treeResult.topHead();
			int  typeSymAttribs = treeTypeSymbol.getAttributes();
			int iastSize = treeResult.size();
		//	logInfo("treeSize=" + iastSize + ", treeTypeSymbol= " + treeTypeSymbol + ", attribs=" + Integer.toHexString(typeSymAttribs) + 
		//				", evalFlags=" + Integer.toHexString(treeEvalFlags) + ", treeResult=" + treeResult);
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

	public Vector3f readVec3f(String expr) {
		// TODO: Set up a reusable double buffer, and allow a Vector3f to be passed in for update.
		
		// It is OK to use an object-level buffer as long as we can truly assume single-threaded access.
		// Otherwise we need to synchronize (which carrys a penalty) or get wacky.
		// Also, allow pre-parsed IExpressions to be cached.
		
		double[] dvals = readDoubleVec(expr, null);
		if (dvals.length == 3) {
			return new Vector3f((float) dvals[0], (float) dvals[1], (float) dvals[2]);
		} else {
			return null;
		}
	}
	
	public void putVar(String name, Object var) {
		myMathEng.put(name, var);
	}

	private static void registerFuncPackage(String pkgName) {
		SystemNamespace.DEFAULT.add(pkgName);
	}	
}
/**
 * MathScriptEngine.java
 * public Object eval(final String script, final ScriptContext context) throws ScriptException {
 *              final ArrayList<ISymbol> list = new ArrayList<ISymbol>();
                try {
                        // first assign the EvalEngine to the current thread:
                        fUtility.startRequest();

                        final Bindings bindings = context.getBindings(ScriptContext.ENGINE_SCOPE);
                        ISymbol symbol;
                        for (Map.Entry<String, Object> currEntry : bindings.entrySet()) {
                                symbol = F.$s(currEntry.getKey());
                                symbol.pushLocalVariable(Object2Expr.CONST.convert(currEntry.getValue()));
                                list.add(symbol);
                        }

                        // evaluate an expression
                        final Object stepwise = get("STEPWISE");
                        IExpr result;
                        if (Boolean.TRUE.equals(stepwise)) {
                                result = fUtility.evalTrace(script, null, F.List());
                        } else {
                                result = fUtility.evaluate(script);
                        }
                        final Object returnType = context.getAttribute("RETURN_OBJECT");
                        if ((returnType != null) && returnType.equals(Boolean.TRUE)) {
                                // return the object "as is"
                                return result;
                        } else {
                                // return the object as String representation
                                if (result.equals(F.Null)) {
                                        return "";
                                }
                                final StringBufferWriter buf = new StringBufferWriter();
                                OutputFormFactory.get().convert(buf, result);
                                // print the result in the console
                                return buf.toString();
                        }

 */