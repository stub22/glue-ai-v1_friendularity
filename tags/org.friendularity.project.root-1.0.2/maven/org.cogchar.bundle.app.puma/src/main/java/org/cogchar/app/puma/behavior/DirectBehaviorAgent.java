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
import java.util.Properties;
import org.appdapter.core.name.Ident;
import org.cogchar.api.thing.WantsThingAction;
import org.cogchar.app.puma.registry.PumaRegistryClient;
import org.cogchar.app.puma.registry.ResourceFileCategory;
import org.cogchar.bind.rk.robot.client.RobotAnimClient;
import org.cogchar.bind.rk.robot.svc.RobotServiceContext;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.impl.perform.FancyTextPerfChan;
import org.cogchar.impl.scene.Theater;
import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
import org.osgi.framework.BundleContext;
import org.jflux.impl.services.rk.osgi.OSGiUtils;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class DirectBehaviorAgent extends PumaBehaviorAgent {
	private PumaRobotMotionMapper myRobotMotionMapper;
	private PumaSpeechOutputMapper mySpeechOutputMapper;

	public DirectBehaviorAgent(Ident agentID, BehaviorConfigEmitter bce)  {
		super(agentID, bce);
	}

	public void initMappers(PumaRegistryClient prc, RobotServiceContext rsc) {
		List<ClassLoader> clsForRKConf = prc.getResFileCLsForCat(ResourceFileCategory.RESFILE_RK_CONF);
		myRobotMotionMapper = new PumaRobotMotionMapper(myAgentID, myBehaviorCE, clsForRKConf, rsc);
		mySpeechOutputMapper = new PumaSpeechOutputMapper(myAgentID);
	}

	public void connectSpeechOutputSvcs(BundleContext bundleCtx, Theater thtr) {
		try {
			mySpeechOutputMapper.connectSpeechOutputSvcs(bundleCtx, thtr);
		} catch (Throwable t) {
			getLogger().error("Cannot connect speech output", t);
		}
	}
	public void connectAnimOutChans(Theater thtr) {
		FancyTextPerfChan bestAnimOutChan = myRobotMotionMapper.getBestAnimOutChan();
		getLogger().warn("Found bestAnimOutChan {}", bestAnimOutChan);
		thtr.registerPerfChannel(bestAnimOutChan);

        BundleContext context = OSGiUtils.getBundleContext(WantsThingAction.class);
        if(context != null && WantsThingAction.class.isAssignableFrom(bestAnimOutChan.getClass())){
            Properties props = new Properties();
            props.put("thingActionChannelAgentId", myAgentID.getAbsUriString());
            context.registerService(WantsThingAction.class.getName(), bestAnimOutChan, (java.util.Dictionary)props);
        }
	}
	public void playBuiltinAnimNow(RobotAnimClient.BuiltinAnimKind baKind) {
		myRobotMotionMapper.playBuiltinAnimNow(baKind);
	}

	public void sayTextNow(String txt) {
		mySpeechOutputMapper._directlyStartSpeakingText(txt);
	}

	@Override protected void doWiringPreStart(BundleContext optBunCtxForWiring, Theater thtr) {
		super.doWiringPreStart(optBunCtxForWiring, thtr);
	}
	@Override protected void doWiringPostStart(BundleContext optBunCtxForWiring, Theater thtr) {
		super.doWiringPostStart(optBunCtxForWiring, thtr);
		// Will currently be null on "crude" restarts.
		if (optBunCtxForWiring != null) {
			connectAnimOutChans(thtr);
			connectSpeechOutputSvcs(optBunCtxForWiring, thtr);
		}
	}
	@Override protected void doStopOutputs () {
		super.doStopOutputs();
		getLogger().info("stopEverything for {} - Stopping Anim Jobs.", getAgentID());
		myRobotMotionMapper.stopAndReset();
		getLogger().info("stopEverything for {} - Stopping Speech-Output Jobs.", getAgentID());
		mySpeechOutputMapper.stopAllSpeechOutput();
	}
	@Override protected void doResetOutputs() {
		getLogger().info("stopResetAndRecenter - Starting GOTO_DEFAULTS anim");
		myRobotMotionMapper.playBuiltinAnimNow(RobotAnimClient.BuiltinAnimKind.BAK_GOTO_DEFAULTS);
	}
}
