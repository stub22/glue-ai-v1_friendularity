package org.friendularity.cpump

import akka.actor.ActorSystem
import akka.osgi.ActorSystemActivator
import org.appdapter.core.log.BasicDebugger
import org.appdapter.fancy.log.VarargsLogging
import org.osgi.framework.BundleContext
import org.slf4j.Logger

/**
  * Created by Stub22 on 7/26/2016.   (Code copied and modified from our CCMIO project).
  */
trait AkkaOSGiHelp extends VarargsLogging {
	private var myAkkaSys_opt: Option[ActorSystem] = None
	private lazy val ourAkkaActivator: OurAkkaOSGiActivator = makeAkkaActivSingleton

	def getFixedAkkaSysName : String

	def startAkkaOSGi(bctx: BundleContext) {
		myAkkaSys_opt = ourAkkaActivator.launchAkkaActorSys(bctx)
	}

	private def makeAkkaActivSingleton: OurAkkaOSGiActivator = {
		val ourActivLogger: Logger = BasicDebugger.getLoggerForClass(classOf[OurAkkaOSGiActivator])
		val ourActivVL = new VarargsLogging {
			override val myLogger: Logger = ourActivLogger
		}
		val ourAkkaSysName = getFixedAkkaSysName
		new OurAkkaOSGiActivator(ourAkkaSysName, ourActivVL)
	}
	def getAkkaSys_opt : Option[ActorSystem] = myAkkaSys_opt
	def getAkkaSys_orNull : ActorSystem = myAkkaSys_opt.getOrElse(null)
}
class AkkaOSGiLaunchHelper(fixedAkkaSysName : String) extends AkkaOSGiHelp {
	override def getFixedAkkaSysName: String = fixedAkkaSysName

}

// Note that this activator extends an akka class which implements  org.osgi.framework.BundleActivator,
// but we do *not* use it as a bundle activator, presently.  Instead we manually call .start() on
// it.  See its complete method list at bottom
class OurAkkaOSGiActivator(myAkkaSysName : String, myVLog : VarargsLogging)
			extends ActorSystemActivator {

	private var myActorSys_opt: Option[ActorSystem] = None

	myVLog.info1("OurAkkaOSGiActivator constructor for akkaSysName={}", myAkkaSysName)

	override def getActorSystemName(bctx: BundleContext): String = {
		myVLog.info1("Supplying actorSysName={}", myAkkaSysName)
		myAkkaSysName
	}

	// This may be called to simulate the effect of OSGi bundle launch.
	def launchAkkaActorSys(bctx : BundleContext) : Option[ActorSystem] = {
		myVLog.info3("OurAkkaActivator singleton = {}, actorSysName={}, bctx={} now calling start()", this, myAkkaSysName, bctx)
		try {
			start(bctx)
			val akkaSys_opt = getActorSys_opt
			myVLog.info1("OurAkkaActivator.start completed without exceptions, akkaSys_opt={}", akkaSys_opt)

		}
		catch {
			case t: Throwable => {
				myVLog.error1("Problem starting akka: {}", t)
			}
		}
		getActorSys_opt
	}


	override def configure(bctx: BundleContext, asys: ActorSystem) {
		myVLog.debug2("configure bctx={}, asys={}", bctx, asys)
		if (myActorSys_opt.isDefined) {
			throw new RuntimeException("Whoah there!  OurAkkaOSGiActivator.configure() got called a second time.")
		}
		myActorSys_opt = Some(asys)
		myVLog.info1("Registering akka system as an OSGi service, akkaSys={}", asys)
		registerService(bctx, asys)
		myVLog.debug1("Akka OSGi-service registration complete for akkaSys={}", asys)
	}

	def getActorSys_opt: Option[ActorSystem] = {
		if (myActorSys_opt.isEmpty) {
			myVLog.warn0("Attempting to fetch actorSys before it is created, returning null")
		}
		myActorSys_opt
	}
}
/*
package akka.osgi
abstract class ActorSystemActivator() extends java.lang.Object with org.osgi.framework.BundleActivator {
  def configure(context : org.osgi.framework.BundleContext, system : akka.actor.ActorSystem) : scala.Unit
  def start(context : org.osgi.framework.BundleContext) : scala.Unit = { /* compiled code */ }
  def addLogServiceListener(context : org.osgi.framework.BundleContext, system : akka.actor.ActorSystem) : scala.Unit = { /* compiled code */ }
  def serviceForReference[T](context : org.osgi.framework.BundleContext, reference : org.osgi.framework.ServiceReference[_]) : T = { /* compiled code */ }
  def stop(context : org.osgi.framework.BundleContext) : scala.Unit = { /* compiled code */ }
  def registerService(context : org.osgi.framework.BundleContext, system : akka.actor.ActorSystem) : scala.Unit = { /* compiled code */ }
  def getActorSystemName(context : org.osgi.framework.BundleContext) : scala.Predef.String = { /* compiled code */ }
  def getActorSystemConfiguration(context : org.osgi.framework.BundleContext) : com.typesafe.config.Config = { /* compiled code */ }
}
 */