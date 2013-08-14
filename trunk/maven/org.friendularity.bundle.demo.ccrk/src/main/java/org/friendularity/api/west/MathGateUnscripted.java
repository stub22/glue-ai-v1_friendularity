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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.script.Bindings;
import javax.script.ScriptContext;
import org.matheclipse.core.convert.Object2Expr;
import org.matheclipse.core.eval.EvalEngine;
import org.matheclipse.core.eval.EvalUtilities;
import org.matheclipse.core.expression.F;
import org.matheclipse.core.interfaces.IExpr;
import org.matheclipse.core.interfaces.ISymbol;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 *
 * Uses code + comments copied from symja's "MathScriptEngine"
 */
public class MathGateUnscripted extends MathGate {
	private EvalEngine	 myEvalEngine;
	private EvalUtilities myEvalUtilityWrapper;
	private Map<String, Object> myVarBindings = new HashMap<String, Object>();
	
	private	Map<String,	IExpr> myParsedExprCache = new HashMap<String, IExpr>();

	public MathGateUnscripted() {
		// Symja comment:   get the thread local evaluation engine
		myEvalEngine = new EvalEngine();
		// engine.setIterationLimit(10);
		myEvalUtilityWrapper = new EvalUtilities(myEvalEngine, false);
	}

	@Override public void putVar(String name, Object var) {
		myVarBindings.put(name, var);
		int bindingMapSize = myVarBindings.size();
		if ((bindingMapSize % 100) == 0) {
			getLogger().warn("Binding Map Size is now: " + bindingMapSize);
		}		
	}
	
	public List<ISymbol> pushValuesForBoundSymbols() {
		
		final ArrayList<ISymbol> pushedSymsToPopLater = new ArrayList<ISymbol>(myVarBindings.size());
		
	// * Assign the associated EvalEngine to the current thread. Every subsequent
	// * action evaluation in this thread affects the EvalEngine in this class.		
		
		myEvalUtilityWrapper.startRequest();
		
		for (Map.Entry<String, Object> currEntry : myVarBindings.entrySet()) {
			ISymbol symbol = F.$s(currEntry.getKey());
			// Now we must trust the caller to pop these values!
			symbol.pushLocalVariable(Object2Expr.CONST.convert(currEntry.getValue()));
			pushedSymsToPopLater.add(symbol);
		}

		return pushedSymsToPopLater;
	}
	public IExpr parseExpression(String expr) { 
		return myEvalEngine.parse(expr);
	}


	
	@Override public IExpr parseAndEvalExprToIExpr(String exprText) {
		IExpr resultExpr = null;
		List<ISymbol> pushedSymsToPopLater = null;
		try {
			//Don't know yet how the parsing process is specifically relying on the symbols,
			// but in MathScriptEngine.eval(), the binding happens before the parsing, 
			// so we repeat that structure.
			pushedSymsToPopLater = pushValuesForBoundSymbols();

			IExpr cachedExpr = myParsedExprCache.get(exprText);
			if (cachedExpr == null) {
				cachedExpr = parseExpression(exprText);
				if (cachedExpr != null) {
					myParsedExprCache.put(exprText, cachedExpr);
					int exprCachSize = myParsedExprCache.size();
					if ((exprCachSize % 100) == 0) {
						getLogger().warn("Expr Cache Size is now: " + exprCachSize);
					}
				} 
			}
			if (cachedExpr != null) {
			//	if (Boolean.TRUE.equals(stepwise)) {
			//		result = fUtility.evalTrace(script, null, F.List());
			//	} else {
					resultExpr = myEvalUtilityWrapper.evaluate(cachedExpr);

			} else {
				getLogger().error("Cannot parse expr [" + exprText + "]");
			}
		} catch (Throwable t) {
			getLogger().error("Error evaluating expr [" + exprText + "]");
		} finally {
			if (pushedSymsToPopLater != null) {
				for (ISymbol sym : pushedSymsToPopLater) {
					sym.popLocalVariable();
				}
			}
		}
		return resultExpr;
	}

}
