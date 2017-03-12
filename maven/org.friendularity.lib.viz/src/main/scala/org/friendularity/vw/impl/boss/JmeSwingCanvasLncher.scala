package org.friendularity.vw.impl.boss

import java.awt.Image
import javax.swing.JFrame

import akka.actor.{ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.ActorRefCPMsgTeller
import org.friendularity.navui.NavAppCloser
import org.friendularity.vw.impl.sys.VWJdkAwtCanvasMgr

/**
  * Created by Owner on 1/22/2017.
  */
trait JmeSwingCanvasLncher extends VarargsLogging {
	var myJavaCanvasManager_opt :Option[VWJdkAwtCanvasMgr] = Option.empty[VWJdkAwtCanvasMgr]

	protected def launchSwingWrappedCanvas(slfActr : ActorRef,  localActorCtx : ActorContext, fixmeClzrNonSerial : NavAppCloser) : Unit = {
		val resultsTeller = new ActorRefCPMsgTeller(slfActr)
		val jdkSwCanvLauncher = new VWJdkAwtCanvasMgr{}
		jdkSwCanvLauncher.launch(resultsTeller, fixmeClzrNonSerial)
		myJavaCanvasManager_opt = Some(jdkSwCanvLauncher)
	}

	protected def setSwingCanvasBranding(canvasTitle: String,  canvasIconImage : Image) : Unit = {

		if(myJavaCanvasManager_opt.isDefined){
			info2("Setting swing canvas branding, title={}, img={}", canvasTitle, canvasIconImage)
			val javaCanvasManager : VWJdkAwtCanvasMgr  = myJavaCanvasManager_opt.get
			val jFrame_opt: Option[JFrame] = javaCanvasManager.getFrameOption
			if(jFrame_opt.isDefined){
				val jFrame = jFrame_opt.get
				jFrame.setTitle(canvasTitle)
				jFrame.setIconImage(canvasIconImage)
			}else{
				error1("Cannot set swing canvas branding with title {}. jFrame does not exist.", canvasTitle)
			}
			error1("Cannot set swing canvas branding with title {}. javaCanvasManager does not exist.", canvasTitle)
		}

	}
}