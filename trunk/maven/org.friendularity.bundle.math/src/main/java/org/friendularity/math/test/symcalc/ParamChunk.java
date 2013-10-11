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

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.bind.symja.MathGateUnscripted;
import org.matheclipse.core.interfaces.IAST;
import org.matheclipse.core.interfaces.IExpr;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class ParamChunk  {

// 	public String mySymbols[] = {"sym00", "sym01", "sym02", "sym03", "sym04", "sym05", "sym06", "sym07", "sym08", "sym09"}; 
	public String mySymPrefix = "$sym0";
	public void pushToMathGate(MathGate mg) {		
	}
	public String getSymName(int symIndex) { 
		// TODO - format with digit numbers
		return mySymPrefix + symIndex;
	}
	
	public static class Text extends ParamChunk {
		public	String		txt_00, txt_01, txt_02, txt_03, txt_04, txt_05, txt_06, txt_07, txt_08, txt_09;
		@Override public void pushToMathGate(MathGate mg) {
			// mg.
		}
		public void post(MathGate mg) { 
			// Presumably the string causes side-effects by defining variables or patterns in the mathGate.
		}
		public IExpr[] parse(MathGate mg) { 
			return new IExpr[10];
			// parseCachableExpr
		}
		public IExpr[] parseAndEval(MathGate mg) { 
			return new IExpr[10];
		}
		public double[][] parseAndEvalToMatrix(MathGate mg, int colCnt) { 
			return new double[10][colCnt];
		}		
	}
	public static class Number extends ParamChunk {
		// This produces values that are initially null, and not (easily?) editable from Whackamole.
		// public	Double		num_00 = 0.0, num_01 = 0.0, num_02, num_03 = 0.0, num_04, num_05, num_06, num_07, num_08, num_09;
		// Here we are implicitly boxing into a Double on each push:
		public double num_00, num_01, num_02, num_03, num_04, num_05, num_06, num_07, num_08, num_09;
		@Override public void pushToMathGate(MathGate mg) {
			mg.putVar(getSymName(0), num_00);
			mg.putVar(getSymName(1), num_01);
			mg.putVar(getSymName(2), num_02);
			mg.putVar(getSymName(3), num_03);
			mg.putVar(getSymName(4), num_04);
			mg.putVar(getSymName(5), num_05);
			mg.putVar(getSymName(6), num_06);
			mg.putVar(getSymName(7), num_07);
			mg.putVar(getSymName(8), num_08);
			mg.putVar(getSymName(9), num_09);
		}
	}

}
