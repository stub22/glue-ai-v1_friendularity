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

	def startAkkaOSGi(bctx: BundleContext, configMarkerClz : Class[_]): Unit = {

		// Weird, setting the context classloader doesn't seem to help, whereas calling this
		// from bundle .start *does* help.
		val savedCL: ClassLoader = Thread.currentThread.getContextClassLoader
		val configBundleCL: ClassLoader = configMarkerClz.getClassLoader
		try {
			if (configBundleCL != null) {
				info2("\n====\nSaved old classloader={}\nSetting contextClassLoader to: {}", savedCL, configBundleCL)
				Thread.currentThread.setContextClassLoader(configBundleCL)
			}
			info0("Launching akka")
			startAkkaOSGi(bctx)

			info0("\n==========\nFinished launching akka")
		}
		catch {
			case th: Throwable => {
				error1(".startAkkaOSGi() caught exception: ", th)
				th.printStackTrace
			}
		} finally {
			info1("\n==========\nSetting contextClassLoader back to: {} ", savedCL)
			Thread.currentThread.setContextClassLoader(savedCL)
		}
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
// it.  See its complete method list at bottom.

// To get bundle-classloaders to line up it seems necessary to call .start() here from an
// actual bundle.start(), but the reason for that is not obvious from reading akka code.

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