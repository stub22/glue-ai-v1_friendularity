/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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
package org.cogchar.app.puma.web;

import com.hp.hpl.jena.query.Dataset;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.core.store.Repo;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.api.thing.WantsThingAction;
import org.cogchar.impl.perform.basic.AnimLaunchEntityAction;
import org.cogchar.impl.thing.basic.BasicThingActionRouter;
import org.cogchar.api.web.WebAppInterface;
import org.cogchar.api.web.WebEntityAction;
import org.cogchar.app.puma.boot.PumaContextCommandBox;
import org.cogchar.bind.lift.LiftAmbassador;

import org.cogchar.bind.rk.robot.client.AnimMediaHandle;
import org.cogchar.bind.rk.robot.client.AnimOutTrigChan;
import org.cogchar.blob.emit.GlobalConfigEmitter;
import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.render.app.trigger.SceneActions;
import org.osgi.framework.BundleContext;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.OSGiUtils;
import org.jflux.impl.services.rk.osgi.ServiceClassListener;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
/**
 * Able to wire and start both our current HTTP services:  1) SPARQL-HTTP repo and 2) Lifter WebUI
 * @author Stu B. <www.texpedient.com>
 */
public class PumaWebMapper extends BasicDebugger {
	
	private CommandTargetForUseFromWeb		myCmdTargetForWeb; // The LiftInterface allows Lift app to hook in and trigger cinematics

	private OSGiComponent					myLiftAppComp;
	private OSGiComponent					myLiftSceneComp;
	private PumaContextCommandBox			myPCCB;
	
	// Make default constuctor private to prevent PumaWebMapper from being instantiated without a PumaAppContext
	private PumaWebMapper() {}
	
	public PumaWebMapper(PumaContextCommandBox pccb) {
		myPCCB = pccb;
	}
	public PumaContextCommandBox getCommandBox() { 
		return myPCCB;
	}
	/**
	 * 
	 * @return 
	 * Called only from	connectLiftInterface()  
	 */
	protected CommandTargetForUseFromWeb geWebCommandTarget() {
		if (myCmdTargetForWeb == null) {
			myCmdTargetForWeb = new CommandTargetForUseFromWeb(myPCCB, this);
		}
		return myCmdTargetForWeb;
	}

//	public void connectLiftSceneInterface(BundleContext bundleCtx) {
//		if (myLiftSceneComp == null) {
//			ServiceLifecycleProvider lifecycle = new SimpleLifecycle(SceneActions.getLauncher(), WebAppInterface.WebSceneInterface.class);
//			myLiftSceneComp = new OSGiComponent(bundleCtx, lifecycle);
//		}
//		myLiftSceneComp.start();
//	}
//
//	public void disconnectLiftSceneInterface(BundleContext bundleCtx) {
//		myLiftSceneComp.stop();
//	}
/**
 * 
 * @param bundleCtx 
 * Called only from 
	 * PumaAppContext.connectWeb(), which is called from PUMA booter, typically from a Framework-started event
	 * handler in some "top" application bundle.
 */
	public void connectLiftInterface(BundleContext bundleCtx) {
		CommandTargetForUseFromWeb webCmdTarget = geWebCommandTarget();
		ServiceLifecycleProvider lifecycle = new SimpleLifecycle(webCmdTarget, LiftAmbassador.LiftAppInterface.class);
		myLiftAppComp = new OSGiComponent(bundleCtx, lifecycle);
		myLiftAppComp.start();
	}
	// 
/**
 * Tell the lifter lifecycle to start, once its OSGi dependencies are satisfied.
 * So far, it is only called from Friendularity tests:  o.f.b.lifter - Activator and o.f.b.repo - Activator.
 * @param bunCtx 
 */
	public void startLifterLifecycle(BundleContext bunCtx) { 
		LifterLifecycle lifecycle = new LifterLifecycle();
    	OSGiComponent lifterComp = new OSGiComponent(bunCtx, lifecycle);
    	lifterComp.start();
	}	
/**
 * Called from o.f.b.repo - Activator 
 * and from 
 * o.c.b.bind.joseki - o.c.joswrap.RepoJosDsetDesc
 * @return 
 */
	public Dataset getMainSparqlDataset() {
		PumaContextCommandBox pccb = getCommandBox();
		RepoClient mainConfRC = pccb.getMainConfigRepoClient();
		Repo mainConfRepo = mainConfRC.getRepo();
		Dataset mainConfDset = mainConfRepo.getMainQueryDataset();
		// Print out the available graphs, for debugging.
		java.util.List<Repo.GraphStat> gStats = mainConfRepo.getGraphStats();
		for (Repo.GraphStat gStat : gStats) {
			getLogger().debug("Found in main config:  " + gStat);
		}		
		return mainConfDset;
	}
	/**
	 * Called only from  GruesomeTAProcessingFuncs.registerActionConsumers()
	 * @param router
	 * @param rc
	 * @param gce 
	 */
	public void registerActionConsumers(BasicThingActionRouter router, RepoClient rc, GlobalConfigEmitter gce) { 

		Ident worldConfigIdent = new FreeIdent("if/exception/while/reading/this/ident/report#null");
		try {
			// We shouldn't have more than one, so let's just assume there's one. This is a slightly different assumption
			// to what happens in PumaVirtualWorldMapper.
			worldConfigIdent = gce.entityMap().get(EntityRoleCN.VIRTUAL_WORLD_ENTITY_TYPE).get(0);
			Ident graphIdent = gce.ergMap().get(worldConfigIdent).get(EntityRoleCN.THING_ACTIONS_BINDINGS_ROLE);
			
			WebEntityAction.Consumer weaConsumer = new WebEntityAction.Consumer();
			router.appendConsumer(graphIdent, weaConsumer);
			BundleContext context = OSGiUtils.getBundleContext(WantsThingAction.class);
            if(context != null){
                String filter = null;//OSGiUtils.createFilter("thingActionChannelAgentId", "*aZR50");
                new TAConsumerTracker(context, filter, router, graphIdent).start();
            }
		} catch (Exception e) {
			getLogger().error("Could not register ThingActionConsumer for config {}", worldConfigIdent.getLocalName(), e);
		}		
	}
    
    static class TAConsumerTracker extends ServiceClassListener<WantsThingAction> {
        private BasicThingActionRouter myRouter;
        private Ident myGraphIdent;
        
        public TAConsumerTracker(BundleContext context, String serviceFilter, BasicThingActionRouter router, Ident graphIdent) {
            super(WantsThingAction.class, context, serviceFilter);
            myRouter = router;
            myGraphIdent = graphIdent;
        }

        @Override
        protected void addService(WantsThingAction t) {
			myRouter.appendConsumer(myGraphIdent, t);
        }

        @Override
        protected void removeService(WantsThingAction t) {
        }
        
        
    }
    
}
