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

import java.util.ArrayList;
import java.util.List;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaRegistryClient;
import org.cogchar.app.puma.body.PumaBodyGateway;
import org.cogchar.app.puma.body.PumaDualBody;
import org.cogchar.bind.rk.robot.svc.*;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.osgi.framework.BundleContext;

import org.cogchar.impl.channel.AnimFileSpecReader;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * 2013-06-10    This class represents a big chunk of the  assumptions used when currently launching our Avatar demos,
 * which do not yet make use of the Lifecycle pattern for loading up services - in contrast to the BehavMasterDemo.
 * 
 * Note the literal constant QNames used below.
 */

public class PumaBehaviorManager extends BasicDebugger {
	private List<PumaBehaviorAgent>		myBehaviorAgentList = new ArrayList<PumaBehaviorAgent>();
	
	private	BehaviorConfigEmitter	myBehavCE;
	
	public void initConfigLinks(PumaRegistryClient prc) { 
		PumaContextMediator mediator = prc.getCtxMediator(null);
		// These values trickle through:
		// BehaviorConfigEmitter, DirectBehaviorAgent, PumaRobotMotionMapper, DirectRobotAnimContext, 
		// and finally into the AnimMediaHandle.Cache  which is used by the AnimOutTrigChan.
		
		RepoClient animResRepoClient = prc.getConfigMgr(null).getMainConfigRepoClient();
		Ident animPathModelID =  animResRepoClient.makeIdentForQName(AnimFileSpecReader.animGraphQN());
		myBehavCE = new BehaviorConfigEmitter(animResRepoClient, animPathModelID);
		
		// Here are some older config properties serving a related config/anim path-resolving role.
		String sysContextURI = mediator.getSysContextRootURI();
		if (sysContextURI != null) {
			myBehavCE.setSystemContextURI(sysContextURI);
		}
		String filesysRootPath = mediator.getOptionalFilesysRoot();
		if (filesysRootPath != null) {
			myBehavCE.setLocalFileRootDir(filesysRootPath);
		}		
	}
	
	public void makeAgentForBody(BundleContext bunCtx, PumaRegistryClient pRegCli, PumaDualBody pdb, Ident agentID) { 
		
		DirectBehaviorAgent pbAgent = new DirectBehaviorAgent(agentID, myBehavCE);
		RobotServiceContext  optLocalRobotSvcCtx = null;
		PumaBodyGateway pBodGate = pdb.getBodyGateway();
		if (pBodGate != null) {
			optLocalRobotSvcCtx = pBodGate.getRobotServiceContext();
		}
		pbAgent.initMappers(pRegCli, optLocalRobotSvcCtx);
		String chanGraphQN =  "ccrt:chan_sheet_AZR50",  behavGraphQN  =  "hrk:behav_file_44";
		// See stack trace below for a good clue as to what this method accomplishes:
		pbAgent.setupAndStart(bunCtx, pRegCli, chanGraphQN, behavGraphQN);
		
		myBehaviorAgentList.add(pbAgent);
	}
	public void stopAllAgents() {
		for (PumaBehaviorAgent pba : myBehaviorAgentList) {
			pba.stopEverything();
		}
	}
}
	/* If  BehaviorConfigEmitter's RepoClient is null in init() above, we will later get:
	 *      [java] 	at org.cogchar.blob.emit.BehaviorConfigEmitter.getAnimPathResolverModel(BehaviorConfigEmitter.scala:33)
     [java] 	at org.cogchar.bind.rk.robot.client.RobotAnimContext.makeMediaHandleCache(RobotAnimContext.java:148)
     [java] 	at org.cogchar.bind.rk.robot.client.RobotAnimContext.getTriggeringChannel(RobotAnimContext.java:141)
     [java] 	at org.cogchar.app.puma.behavior.PumaRobotMotionMapper.getBestAnimOutChan(PumaRobotMotionMapper.java:76)
     [java] 	at org.cogchar.app.puma.behavior.DirectBehaviorAgent.connectAnimOutChans(DirectBehaviorAgent.java:56)
     [java] 	at org.cogchar.app.puma.behavior.DirectBehaviorAgent.doWiringPostStart(DirectBehaviorAgent.java:75)
     [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorAgent.startTheater(PumaBehaviorAgent.java:91)
     [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorAgent.setupAndStartBehaviorTheater(PumaBehaviorAgent.java:81)
     [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorAgent.setupAndStart(PumaBehaviorAgent.java:60)
     [java] 	at org.cogchar.app.puma.behavior.PumaBehaviorManager.makeAgentForBody(PumaBehaviorManager.java:72)
     [java] 	at org.cogchar.app.puma.boot.PumaAppContext.makeAgentsForAllBodies(PumaAppContext.java:180)
     [java] 	at org.cogchar.app.puma.boot.PumaAppContext.connectAllBodies(PumaAppContext.java:168)
     [java] 	at org.cogchar.app.puma.boot.PumaBooter.pumaBootUnsafeUnderOSGi(PumaBooter.java:144)
     [java] 	at org.cogchar.app.puma.boot.PumaBooter.bootUnderOSGi(PumaBooter.java:77)
     [java] 	at com.hrkind.bundle.opengl.R50.ActivatorPumaOpenglR50.startPumaDemo(ActivatorPumaOpenglR50.java:103)
     [java] 	at com.hrkind.bundle.opengl.R50.ActivatorPumaOpenglR50.handleFrameworkStartedEvent(ActivatorPumaOpenglR50.java:66)

		 */
