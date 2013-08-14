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

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import org.matheclipse.core.interfaces.IExpr;
import org.matheclipse.script.engine.MathScriptEngine;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class MathGateScripted extends MathGate {
	ScriptEngine	myMathEng;
	
	public MathGateScripted(ScriptEngine mathScriptEngine) {
		myMathEng = mathScriptEngine;
		enableTreeResults();
		final Object stepwise = mathScriptEngine.get("STEPWISE");
		getLogger().warn("stepwise =" + stepwise);
	}
	public void enableTreeResults() { 
		ScriptContext context = myMathEng.getContext();
		// If we do this, an AST is returned.  Otherwise a String is returned.
		context.setAttribute(MathScriptEngine.RETURN_OBJECT, Boolean.TRUE,	ScriptContext.ENGINE_SCOPE);
	}	
	@Override public IExpr parseAndEvalExprToIExpr(String expr) {
		IExpr result = null;
		try {
			result = (IExpr) myMathEng.eval(expr);
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return result;
	}
	public void putVar(String name, Object var) {
		myMathEng.put(name, var);
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