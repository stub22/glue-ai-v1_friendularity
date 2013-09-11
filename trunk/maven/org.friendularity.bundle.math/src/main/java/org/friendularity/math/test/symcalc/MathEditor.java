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



import org.appdapter.api.trigger.ABoxImpl;
import org.appdapter.api.trigger.Box;
import org.appdapter.api.trigger.Trigger;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.gui.demo.DemoBrowser;
import org.friendularity.math.api.MathGate;
import org.friendularity.math.api.MathGateUnscripted;
import org.friendularity.math.api.MathSpaceFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class MathEditor extends BasicDebugger {

	public ParamChunk myNumChunk = new ParamChunk.Number();
	public ParamChunk myTxtChunk = new ParamChunk.Text();
	public CalcParamChunk myCalcChunk = new CalcParamChunk();
	
	public void doCalcs(MathGateUnscripted mgu) {
		myCalcChunk.doCalcs(mgu);
	}

	public static void main(final String args[]) {
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		org.apache.log4j.BasicConfigurator.configure();	
		// org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		final MathEditor editor = new MathEditor();
		MathSpaceFactory msf = new MathSpaceFactory();
		final MathGateUnscripted mgu = msf.makeUnscriptedMathGate();
		DemoBrowser.showObject("aMathEditor", editor, false, false);
		// werm.setMathGate(mg);

		DemoBrowser.registerTriggerForObject(editor, "doCalcsOnce", new Trigger() {
			@Override public void fire(Box box) {
				editor.getLogger().info("Someone clicked aMathEditor and it came back Boxed up in object of type: " 
						+ box.getClass() + ", shortLabel="	+ ((ABoxImpl) box).getShortLabel() + ", full dump: " + box);
				editor.doCalcs(mgu);
				// assert(box.getValue()==myTestRepo);
			}
		});

	}
}
