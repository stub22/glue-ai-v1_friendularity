/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.bundle.demo.ccmio;

// import akka.actor.Props;
import akka.actor.ActorSystem;
import org.friendularity.infra.akact.AkkaOSGiLaunchHelper;
import org.friendularity.infra.cpmsg.CPMsgTeller;

import org.friendularity.infra.cpmsg.TxtSymMsg;
import org.friendularity.qth.DemoCPumpMgr;
import org.friendularity.qth.PluginDemoCPumpMgr;
import org.osgi.framework.BundleContext;

/**
 * Created by Owner on 3/29/2016.
 */
public class CCMIO_CPumpHelper extends AkkaOSGiLaunchHelper {

	private DemoCPumpMgr myDCPM;

	/*
	// http://doc.akka.io/docs/akka/2.3.14/additional/osgi.html
	// Instantiating this (or invoking start()?)  triggers Akka bundle scan setup ... right?
	private static OurAkkaOSGiActivator ourAkkaActivator = null;
*/
	public static String ourAkkaSysName = "ccmioBundle";

	public static String ourPumpName = "ccmioPump";

	public CCMIO_CPumpHelper() {
		super(ourAkkaSysName);
	}

	/*
		private static OurAkkaOSGiActivator getAkkaActivSingle() {
			if (ourAkkaActivator == null) {

				Logger ourActivLogger = getLoggerForClass(OurAkkaOSGiActivator.class);
				ourAkkaActivator = new OurAkkaOSGiActivator(ourAkkaSysName, ourActivLogger);
			}
			return ourAkkaActivator;
		}
		private ActorSystem myAkkaSys = null;
		public void startAkkaOSGi(BundleContext bctx) {
			OurAkkaOSGiActivator akkaActiv = getAkkaActivSingle();
			String actorSysName = akkaActiv.getActorSystemName(bctx);
			getLogger().info("OurAkkaActivator singleton = {}, actorSysName={}, now calling start() on it", akkaActiv, actorSysName);
			try {
				akkaActiv.start(bctx);
				myAkkaSys = akkaActiv.getActorSys();
				getLogger().info("OurAkkaActivator.start completed without exceptions, myAkkaSys={}", myAkkaSys);
			} catch (Throwable t) {
				logError("Problem starting akka: {}", t);
			}
		}
	*/
	public boolean launchCPump(BundleContext bctx) {
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  CCMIO_CPumpHelper.launchCPump()-START");
		ActorSystem akkaSys_orNull = getAkkaSys_orNull();
		if (akkaSys_orNull == null) {
			getLogger().info("No akka sys found, aborting CPump launch");
			return false;
		}
		myDCPM = new PluginDemoCPumpMgr(akkaSys_orNull, ourPumpName);
		// val cpumpActorRef : ActorRef = myDCPM.getCPumpActRef
		// Typical result dumps as   Actor[akka://demoCPAS/user/demoCPump01#618243248]
		// info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpServer. main() - got initial cpumpActorRef: {}", cpumpActorRef);
		// Establish handler so that death of cpumpActor triggers end of the ActorSystem, which
		// closes up threads and possibly allows java process to exit.
		myDCPM.connectCPumpActorSystemTerminator();

		// Goal is to receive, act upon, and reply to messages sent from the TestCPumpClients,
		// using any glue.ai message pathway:  local akka, remote akka, spray HTTP, Camel QPid

		TxtSymMsg tsm01 = new TxtSymMsg("First contents");

		CPMsgTeller rootTeller = myDCPM.getRootTeller();
		rootTeller.tellCPMsg(tsm01);

		TxtSymMsg tsm02 = new TxtSymMsg("Second  contents");

		rootTeller.tellCPMsg(tsm02);
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  CCMIO_CPumpHelper.launchCPump()-END");
		return true;
	}
	public ActorSystem dangerActorSysExposed() {
		return getAkkaSys_orNull();
	}
/*
	// TODO:  Implement an interface used by CPump to access OSGi-akka hooks as needed.
	private static class OurAkkaOSGiActivator extends ActorSystemActivator {
		private Logger myLogger;
		private String myAkkaSysName = null;
		private ActorSystem myActorSys = null;
		public OurAkkaOSGiActivator(String akkaSysName, Logger log) {
			myAkkaSysName = akkaSysName;
			myLogger = log;
			log.info("CPump.OurAkkaOSGiActivator constructor for akkaSysName={}", akkaSysName);
		}
		@Override public String getActorSystemName(BundleContext bctx) {
			myLogger.info("Supplying actorSysName={}", myAkkaSysName);
			return myAkkaSysName;
		}
		@Override public void  configure(BundleContext bctx, ActorSystem asys)  {
			myLogger.info("configure bctx={}, asys={}", bctx, asys);
			// Keep a local handle to the ActorSys
			myActorSys = asys;
			// optionally register the ActorSystem in the OSGi Service Registry
			registerService(bctx, asys);

			//	val someActor = system.actorOf(Props[SomeActor], name = "someName")
			//	someActor ! SomeMessage
		}
		public ActorSystem getActorSys() {
			if (myActorSys == null) {
				myLogger.warn("Attempting to fetch actorSys before it is created, returning null");
			}
			return myActorSys;
		}

	}
*/
}

