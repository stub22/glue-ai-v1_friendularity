/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

package org.cogchar.app.puma.behavior;

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.cogchar.bind.rk.speech.client.SpeechOutputClient;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.impl.perform.PerfChannelNames;
import org.cogchar.impl.scene.Theater;
import org.osgi.framework.BundleContext;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * Talks to Robokind Speech Output services using SpeechOutputClient binding.
 */

public class PumaSpeechOutputMapper extends BasicDebugger {
	
	private		Ident						myAgentID;
	private		SpeechOutputClient			mySOC;

	public PumaSpeechOutputMapper(Ident agentID) {
		myAgentID = agentID;
	}
	
	public void connectSpeechOutputSvcs(BundleContext bundleCtx, Theater t) {
		Ident speechChanIdent = PerfChannelNames.getOutChanIdent_SpeechMain();
		
		mySOC = new SpeechOutputClient(bundleCtx, speechChanIdent);
		t.registerPerfChannel(mySOC);
	}
	@Deprecated protected void _directlyStartSpeakingText(String txt) {
		// TODO:  Prevent/blend concurrent activity through the channel/behavior systerm
		try {
			if (mySOC != null) {
				mySOC._directlyStartSpeakingText(txt);
			} else {
				getLogger().warn("Character {} ignoring request to sayText, because SpeechOutputClient is null", myAgentID);
			}
		} catch (Throwable t) {
			getLogger().error("problem speaking", t);
		}
	}
	public void stopAllSpeechOutput() { 
		if (mySOC != null) {
			mySOC._directlyCancelAllRunningSpeechTasks();
		}	
	}
	
}
