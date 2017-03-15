package org.friendularity.vw.impl.boss

import akka.actor.{ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.ActorRefCPMsgTeller
import org.friendularity.vw.impl.sys.SimBalloonAppLauncher

/**
  * Created by Owner on 1/22/2017.
  */
trait JmeBareCanvasLncher extends VarargsLogging {
	protected def launchBareJmeCanvas(slfActr : ActorRef,  localActorCtx : ActorContext) : Unit = {
		val bsim = new SimBalloonAppLauncher {}
		val resultsTeller = new ActorRefCPMsgTeller(slfActr) // vwLnchMsg.resultsTeller
		info0("launchBareJmeCanvas Calling bsim.setup")
		bsim.setup(resultsTeller)
		info0("launchBareJmeCanvas END - vworld is now running, but delayed setup jobs may still be running/pending")
	}

}
