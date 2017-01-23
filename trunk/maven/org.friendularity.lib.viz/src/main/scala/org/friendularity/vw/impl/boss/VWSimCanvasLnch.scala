package org.friendularity.vw.impl.boss

import java.awt.Image
import javax.swing.JFrame

import akka.actor.{ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.ActorRefCPMsgTeller
import org.friendularity.navui.NavAppCloser
import org.friendularity.vw.impl.sys.{VWJdkAwtCanvasMgr, SimBalloonAppLauncher}
import org.friendularity.vw.msg.adm.{VWSetSwingCanvasBranding, VWSetupRq_Lnch}

/**
  * Created by Owner on 1/22/2017.
  */

trait VWSimCanvasLnchMgr extends JmeBareCanvasLncher with JmeSwingCanvasLncher {
	def launchSimRenderSpace(vwLnchMsg : VWSetupRq_Lnch, slfActr : ActorRef,  localActorCtx : ActorContext): Unit = {
		// TODO:  We want this launch process to call us back with the ingredients chef needs to proceed.
		// The soonest that *could* happen is *during* JME3.start(), but we would actually prefer it be later,
		// during isolatedInitTask.
		if (vwLnchMsg.wrapInSwingCanv) {

			launchSwingWrappedCanvas(slfActr, localActorCtx, vwLnchMsg.fixmeClzrNonSerial)
		} else {
			launchBareJmeCanvas(slfActr, localActorCtx)
		}
	}
	def handleCanvasBranding( scb : VWSetSwingCanvasBranding): Unit = {
		setSwingCanvasBranding(scb.canvasTitle, scb.canvasIconImage)
	}
}
