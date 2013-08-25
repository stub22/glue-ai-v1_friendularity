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
 * @author Stu B. <www.texpedient.com>
 */

public class MathSpaceFactory {
	static String ENGINE_FACTORY_NAME = "symja_via_appd";
	ScriptEngineManager		myEngineManager;
	
	public MathSpaceFactory() { 
		myEngineManager = new ScriptEngineManager();
		MathScriptEngineFactory mseFactory = new MathScriptEngineFactory();
		// In server mode we are only allowed to change symbols that start with "$".
		// We use this feature to try to darken the line between symbol categories.
		// Note that the constructor for MathScriptEngineFactory sets this flag to false, so we
		// are overriding it here.
		Config.SERVER_MODE = true;
		myEngineManager.registerEngineName(ENGINE_FACTORY_NAME, mseFactory);
	}
	public MathGate makeScriptedMathGate() { 
		ScriptEngine engine = myEngineManager.getEngineByName(ENGINE_FACTORY_NAME);
		MathGate mg = new MathGateScripted(engine);
		return mg;
	}
	public MathGate makeUnscriptedMathGate() { 
		MathGate mg = new MathGateUnscripted();
		return mg;
	}
	
}
