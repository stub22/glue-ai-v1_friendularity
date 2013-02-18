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
import org.matheclipse.core.basic.Config;

import org.matheclipse.core.eval.SystemNamespace;
import org.matheclipse.core.interfaces.IAST;
import org.matheclipse.core.interfaces.IExpr;
import org.matheclipse.script.engine.MathScriptEngine;
import org.matheclipse.script.engine.MathScriptEngineFactory;

/**
 *
 * Adapted from:
 *
 * http://code.google.com/p/symja/wiki/JSR223Support
 *
 * @author Stu B. <www.texpedient.com>
 */
public class ScriptEngineExperiment {

	private ScriptEngine myEng;
	private String mySpaceName;
	static String engineName = "matheclipse";

	public static void main(final String args[]) {
		ScriptEngineManager scriptManager = new ScriptEngineManager();
		MathScriptEngineFactory msef = new MathScriptEngineFactory();
		// In server mode we are only allowed to change symbols that start with "$".
		// We use this feature to try to darken the line between symbol categories.
		// Note that the constructor for MathScriptEngineFactory sets this flag to false, so we
		// are overriding it here.
		Config.SERVER_MODE = true;
		scriptManager.registerEngineName(engineName, msef);
		// Otherwise requires an entry in META-INF/services/javax.script.ScriptEngineFactory
		// ScriptEngine scriptEng = scriptManager.getEngineByExtension("m");
		ScriptEngine scriptEng_1 = scriptManager.getEngineByName(engineName);
		// ScriptEngine scriptEng  = msef.getScriptEngine();
		System.out.println("Got script engine: " + scriptEng_1);
		ScriptEngineExperiment tse1 = new ScriptEngineExperiment();

		tse1.myEng = scriptEng_1;
		tse1.mySpaceName = "ONE";

		// This is a static level call that affects the F registry used by the whole JVM.
		registerFuncPackage("org.friendularity.bundle.symcalc");

		tse1.go(100);

		System.out.println("================================================================================");


		ScriptEngine scriptEng_2 = scriptManager.getEngineByName(engineName);

		ScriptEngineExperiment tse2 = new ScriptEngineExperiment();
		tse2.myEng = scriptEng_2;
		tse2.mySpaceName = "TWO";
		tse2.go(200);
		System.out.println("================================================================================");
		
		tse1.go(111);
	}

	public Object evalPrint(String script) {
		Object result = "";
		try {
			result = myEng.eval(script);
		} catch (Throwable t) {
			System.err.println("Error evaluating: '" + script + "'");
			t.printStackTrace();
		}
		String resultType = "<" + result.getClass().getSimpleName() + "> ";
		System.out.println(mySpaceName + ": '" + script + "' -> " + result + " " + resultType);
		return result;
	}

	public void putVar(String name, Object var) {
		myEng.put(name, var);
	}

	private static void registerFuncPackage(String pkgName) {
		SystemNamespace.DEFAULT.add(pkgName);
	}

	public String dq(String a) {
		return "\"" + a + "\"";
	}
	public void go(int magicMult) {
		try {
			evalPrint("Fit[{{1,1},{2,4},{3,8},{4,17}},3,x]");
			// This "pattern" is only defined below - so it will only work here on a subsequent "go()",
			// assuming no other engine has messed up the F context (which is what we're testing!)
			evalPrint("$threeVec");
			evalPrint("$threeVec[$threeVec[8," + dq("onlyTwoArgs!") + "],5,6]");
			evalPrint("$threeVec[1,2,-3]");
			evalPrint("D[Sin[x]*Cos[x],x]");		//  assertEquals("Cos[x]^2-Sin[x]^2"
			String e = (String) evalPrint("Expand[(x+5)^3]");			//   assertEquals("125+75*x+15*x^2+x^3", stringResult);
			evalPrint("Factor[" + e + "]");		//  assertEquals("(5+x)^3", stringResult);

			// Define variables $x and $y:

			evalPrint("$x");
			putVar("$x", new Boolean(true));
			putVar("$y", new Boolean(true));
			String xAndY = (String) evalPrint("$x && $y");	// assertEquals("True", stringResult);

			evalPrint("$threeVec[v1_,v2_,v3_]:={v1,v2 * " + magicMult + ",v3}");
			evalPrint("$threeVec[$x,5,NextPrime[37,3]]");

			Object ratObj1 = evalPrint("Rational[2,3]");
			Object ratObj2 = evalPrint("Rational[11,5]+2");
			Object ratObj3 = evalPrint("7/9");

			evalPrint("Expand[(A*X^2+B*X)^2]");


			// Here is our own function object being applied. 
			Object wackyObj = evalPrint("WackyFunction[Rational[2,3]+5]");
			
			evalPrint("$threeVec[$x,5,NextPrime[37,3]]");

			ArrayList<Object> row = new ArrayList<Object>();
			row.add("List"); // head of the expression
			row.add(Integer.valueOf(1));
			row.add(Integer.valueOf(2));
			row.add(Integer.valueOf(3));

			int[] intArr = {3, 4, 11};

			putVar("$x", row);
			putVar("$y", intArr);

			Object mval = evalPrint("$m={$x, $y, {13, 7, 8}}");
			/*
			 * '$m={$x, $y, {13, 7, 8}}' -> {{1,2,3},{3,4,11},{13,7,8}} <String> '$m.$m' -> {{46,31,49}, {158,99,141},
			 * {138,110,180}} <String>
			 */
			// putVar("$m", mval);
			// String mSquared = evalPrint
// the test.m file contains this script for matrix multiplication:
// $m={$x, $y, {13, 7, 8}};
// $m.$m

			ScriptContext context = myEng.getContext();
			// If we do this, an AST is returned.  Otherwise a String is returned.
			// context.setAttribute(MathScriptEngine.RETURN_OBJECT, Boolean.TRUE,	ScriptContext.ENGINE_SCOPE);
			Object objectResult = evalPrint("$m.$m");
			//   engine_1         .eval(new FileReader(         "C:\\temp\\test.m"));
// print result for matrix multiplication: {{1,2,3}, {3, 4, 11},
// {13, 7, 8}}.{{1,2,3}, {3, 4, 11}, {13, 7, 8}}
			// assertEquals("{{46, 31, 49}, {158, 99, 141}, {138, 110, 180}}",         objectResult.toString());
			if (objectResult instanceof IExpr) {
				// decompose the matrix into rows
				IExpr expr = (IExpr) objectResult;
				// gives the head "List", because matrices are list of row-lists
				//   assertEquals("List", expr.head().toString());
				printIExpr(expr);
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public void printIExpr(IExpr expr) {

		if (expr instanceof List) {
			System.out.println("----------- Printing List, foreach -------");
			// use java.util.List to print the rows
			List<IExpr> list = (List<IExpr>) expr;
			for (IExpr subExpr : list) {
				System.out.println(subExpr);
			}
			System.out.println("----------- Printing List, for/index -------");
			IExpr subExpr;
			// there's a difference between foreach and for loop
			// because the head is stored at index 0:
			for (int i = 0; i < list.size(); i++) {
				subExpr = list.get(i);
				System.out.println(subExpr);
			}
		}

		if (expr instanceof IAST) {
			System.out.println("----------- Printing IAST, foreach -------");
			// use org.matheclipse.core.interfaces.IAST to print the
			// rows
			IAST list = (IAST) expr;
			for (IExpr subExpr : list) {
				System.out.println(subExpr);
			}
			IExpr subExpr;
			// there's a difference between foreach and for loop
			// because the head is stored at index 0:
			System.out.println("----------- Printing IAST, for/index -------");
			for (int i = 0; i < list.size(); i++) {
				subExpr = list.get(i);
				System.out.println(subExpr);
			}


		}

	}
}
/*
 * Error evaluating: 'Factor[x^3+15*x^2+75*x+125]' java.lang.NoClassDefFoundError: org/apache/log4j/Logger at
 * edu.jas.poly.TermOrder.<clinit>(TermOrder.java:83) at
 * org.matheclipse.core.convert.JASConvert.<init>(JASConvert.java:74) at
 * org.matheclipse.core.reflection.system.Factor.factor(Factor.java:65) at
 * org.matheclipse.core.reflection.system.Factor.evaluate(Factor.java:54) at
 * org.matheclipse.core.eval.EvalEngine.evalASTBuiltinFunction(EvalEngine.java:595) at
 * org.matheclipse.core.eval.EvalEngine.evalASTArg1(EvalEngine.java:444) at
 * org.matheclipse.core.eval.EvalEngine.evalAST(EvalEngine.java:458) at
 * org.matheclipse.core.expression.AST.evaluate(AST.java:1078) at
 * org.matheclipse.core.eval.EvalEngine.evalLoop(EvalEngine.java:770) at
 * org.matheclipse.core.eval.EvalEngine.evalWithoutNumericReset(EvalEngine.java:268) at
 * org.matheclipse.core.eval.EvalEngine.evaluate(EvalEngine.java:287) at
 * org.matheclipse.core.eval.EvalUtilities.evaluate(EvalUtilities.java:45) at
 * org.matheclipse.script.engine.MathScriptEngine.eval(MathScriptEngine.java:84) at
 * javax.script.AbstractScriptEngine.eval(AbstractScriptEngine.java:247) at
 * org.friendularity.bundle.symcalc.TryScriptEngine.evalPrint(TryScriptEngine.java:58) at
 * org.friendularity.bundle.symcalc.TryScriptEngine.go(TryScriptEngine.java:74) at
 * org.friendularity.bundle.symcalc.TryScriptEngine.main(TryScriptEngine.java:53)
 */
