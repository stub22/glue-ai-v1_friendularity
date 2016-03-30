package org.friendularity.bundle.demo.ccmio;

// import akka.actor.Props;
import akka.actor.ActorSystem;
import akka.osgi.ActorSystemActivator;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.cpump.CPMsgTeller;
import org.friendularity.cpump.DemoCPumpMgr;
import org.friendularity.cpump.TxtSymMsg;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

/**
 * Created by Owner on 3/29/2016.
 */
public class CCMIO_CPumpHelper extends BasicDebugger {

	private DemoCPumpMgr myDCPM;

	// http://doc.akka.io/docs/akka/2.3.14/additional/osgi.html
	// Instantiating this triggers Akka bundle scan setup ... right?
	private static OurAkkaOSGiActivator ourAkkaActivator = null;

	private static OurAkkaOSGiActivator getAkkaActivSingle() {
		if (ourAkkaActivator == null) {
			ourAkkaActivator = new OurAkkaOSGiActivator(getLoggerForClass(OurAkkaOSGiActivator.class));
		}
		return ourAkkaActivator;
	}

	public void launchCPump(BundleContext bctx) {
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  CCMIO_CPumpHelper.launchCPump()-START");
		OurAkkaOSGiActivator akkaActiv = getAkkaActivSingle();
		String actorSysName = akkaActiv.getActorSystemName(bctx);
		getLogger().info("OurAkkaActivator singleton = {}, actorSysName={}, now calling start() on it", akkaActiv, actorSysName);
		akkaActiv.start(bctx);
		getLogger().info("OurAkkaActivator.start completed");
		myDCPM = new DemoCPumpMgr();
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
	}
	// TODO:  Implement an interface used by CPump to access OSGi-akka hooks as needed.
	private static class OurAkkaOSGiActivator extends ActorSystemActivator {
		private Logger myLogger;
		public OurAkkaOSGiActivator(Logger log) {
			myLogger = log;
			log.info("CPump.OurAkkaOSGiActivator Constructor yay!");
		}
		public void  configure(BundleContext bctx, ActorSystem asys)  {
			myLogger.info("configure bctx={}, asys={}", bctx, asys);
			// optionally register the ActorSystem in the OSGi Service Registry
			registerService(bctx, asys);

			//	val someActor = system.actorOf(Props[SomeActor], name = "someName")
			//	someActor ! SomeMessage
		}

	}
}

