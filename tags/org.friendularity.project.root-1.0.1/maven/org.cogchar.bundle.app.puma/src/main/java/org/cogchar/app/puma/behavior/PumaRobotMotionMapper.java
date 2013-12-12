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

import java.util.List;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.cogchar.bind.rk.robot.client.*;
import org.cogchar.bind.rk.robot.svc.RobotServiceContext;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.impl.perform.PerfChannelNames;
import org.cogchar.impl.perform.FancyTextPerfChan;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * This object knows how to do fun and useful things with underlying Robokind motion + animation services
 * using the RobotAnimContext and RobotServiceContext (latter is currently unused, except to init the RobotAnimContext).
 */

public class PumaRobotMotionMapper extends BasicDebugger {
	private	Ident				myAnimOutTrigChanID;
	private	RobotServiceContext	myRobotSvcCtx;
	private	RobotAnimContext	myRobotAnimCtx;
	
	public PumaRobotMotionMapper (Ident animOutTrigChanID, BehaviorConfigEmitter behavCE, List<ClassLoader> clsForRKConf,
				RobotServiceContext optLocalRobotSvcContext)  {
		myAnimOutTrigChanID = animOutTrigChanID;
		myRobotSvcCtx = optLocalRobotSvcContext;
		if (behavCE == null) {
			getLogger().warn("Cannot init with behavCE == null");
			return;
		}
		// Set up our animation triggering context, connecting behavior system to scripted-animation system.
		if (optLocalRobotSvcContext != null) {
				// This way is used for direct connect to a local robot graph, bypassing some abstractions.
            try{
                DirectRobotAnimContext drac = new DirectRobotAnimContext(animOutTrigChanID, behavCE, optLocalRobotSvcContext);
                myRobotAnimCtx = drac;
            }catch(RuntimeException ex){
                getLogger().warn("Error creating DirectRobotAnimContext.", ex);
                myRobotAnimCtx = new RobotAnimContext(myAnimOutTrigChanID, behavCE);
            }
		} else {
			// This way is most general case for channel + lifecycle wiring.
			myRobotAnimCtx = new RobotAnimContext(myAnimOutTrigChanID, behavCE);
		}
		if (clsForRKConf != null) {
			// Setup classLoaders used to load animations
			myRobotAnimCtx.setResourceClassLoaders(clsForRKConf);
		}
	}

	/**
	 * This method exposes our "best" AnimOutChan at protected scope.
	 * This is the main pathway for wiring animation triggers from behavior systems
	 * (whether local or remote).
	 * @return 
	 */	
	// Can eventually be dropped in favor of channel-lifecycle wiring
	protected FancyTextPerfChan getBestAnimOutChan() { 
		AnimOutTrigChan aotc = myRobotAnimCtx.getTriggeringChannel();
		Ident chanID = aotc.getIdent();
		Ident bestOutChanID =  PerfChannelNames.getOutChanIdent_AnimBest();
		if (!chanID.equals(bestOutChanID)) {
			AnimOutTrigChan wrappedChan =  new AnimOutTrigChan(bestOutChanID, aotc);
			aotc = wrappedChan;
		}
		return aotc;
	}
	protected void stopAndReset() {
		if (myRobotAnimCtx != null) {
			myRobotAnimCtx.stopAndReset();
		}else {
			getLogger().warn("stopAndReset() ignored because RobotAnimContext = null for {}", myAnimOutTrigChanID);
		}
	}
	protected void playBuiltinAnimNow(RobotAnimClient.BuiltinAnimKind baKind) {
		if (myRobotAnimCtx != null) {
			myRobotAnimCtx.playBuiltinAnimNow(baKind);
		} else {
			getLogger().warn("playDangerYogaTestAnim() ignored because RobotAnimContext = null for {}", myAnimOutTrigChanID);
		}
	}	
}
