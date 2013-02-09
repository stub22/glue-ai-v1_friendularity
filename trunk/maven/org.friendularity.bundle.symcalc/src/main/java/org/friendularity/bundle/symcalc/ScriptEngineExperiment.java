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
package org.friendularity.bundle.symcalc;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.matheclipse.core.eval.SystemNamespace;
import org.matheclipse.core.interfaces.IAST;
import org.matheclipse.core.interfaces.IExpr;
import org.matheclipse.script.engine.MathScriptEngine;
import org.matheclipse.script.engine.MathScriptEngineFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class ScriptEngineExperiment {
	private		ScriptEngine		myEng;
	static String engineName = "matheclipse";
	
	public static void main(final String args[]) {
		ScriptEngineManager scriptManager = new ScriptEngineManager();
		MathScriptEngineFactory msef = new MathScriptEngineFactory();		
		scriptManager.registerEngineName(engineName, msef);
		// Otherwise requires an entry in META-INF/services/javax.script.ScriptEngineFactory
		// ScriptEngine scriptEng = scriptManager.getEngineByExtension("m");
		ScriptEngine scriptEng = scriptManager.getEngineByName(engineName);
		// ScriptEngine scriptEng  = msef.getScriptEngine();
		System.out.println("Got script engine: " + scriptEng);
		ScriptEngineExperiment tse = new ScriptEngineExperiment();
		
		tse.myEng = scriptEng;
		
		
		tse.go();
	}
	public String evalPrint(String script) {
		String result = "";
		try {
			result = (String) myEng.eval(script);
		} catch (Throwable t) {
			System.err.println("Error evaluating: '" + script + "'");
			t.printStackTrace();
		}
		System.out.println("'" + script + "' -> " + result);
		return result;
	}
	public void putVar(String name, Object var) {
		myEng.put(name, var);
	}
	public void go() {
		try {

			evalPrint("D[Sin[x]*Cos[x],x]");		//  assertEquals("Cos[x]^2-Sin[x]^2"
			String e = evalPrint("Expand[(x+5)^3]");			//   assertEquals("125+75*x+15*x^2+x^3", stringResult);
			evalPrint("Factor[" + e + "]");		//  assertEquals("(5+x)^3", stringResult);

			// Define variables $x and $y:

			putVar("$x", new Boolean(true));
			putVar("$y", new Boolean(true));
			String xAndY = evalPrint("$x && $y");	// assertEquals("True", stringResult);

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
}
/*
 * Error evaluating: 'Factor[x^3+15*x^2+75*x+125]'
java.lang.NoClassDefFoundError: org/apache/log4j/Logger
	at edu.jas.poly.TermOrder.<clinit>(TermOrder.java:83)
	at org.matheclipse.core.convert.JASConvert.<init>(JASConvert.java:74)
	at org.matheclipse.core.reflection.system.Factor.factor(Factor.java:65)
	at org.matheclipse.core.reflection.system.Factor.evaluate(Factor.java:54)
	at org.matheclipse.core.eval.EvalEngine.evalASTBuiltinFunction(EvalEngine.java:595)
	at org.matheclipse.core.eval.EvalEngine.evalASTArg1(EvalEngine.java:444)
	at org.matheclipse.core.eval.EvalEngine.evalAST(EvalEngine.java:458)
	at org.matheclipse.core.expression.AST.evaluate(AST.java:1078)
	at org.matheclipse.core.eval.EvalEngine.evalLoop(EvalEngine.java:770)
	at org.matheclipse.core.eval.EvalEngine.evalWithoutNumericReset(EvalEngine.java:268)
	at org.matheclipse.core.eval.EvalEngine.evaluate(EvalEngine.java:287)
	at org.matheclipse.core.eval.EvalUtilities.evaluate(EvalUtilities.java:45)
	at org.matheclipse.script.engine.MathScriptEngine.eval(MathScriptEngine.java:84)
	at javax.script.AbstractScriptEngine.eval(AbstractScriptEngine.java:247)
	at org.friendularity.bundle.symcalc.TryScriptEngine.evalPrint(TryScriptEngine.java:58)
	at org.friendularity.bundle.symcalc.TryScriptEngine.go(TryScriptEngine.java:74)
	at org.friendularity.bundle.symcalc.TryScriptEngine.main(TryScriptEngine.java:53)
 */
