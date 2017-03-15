package org.friendularity.vw.mprt.ingred

import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor

// When used in messages, these ingreds are not fully serializable, and thus may not be passed over
// networking connections.  They generally work OK as messages within a single JVM, however.

trait LesserIngred {
	// Includes pointers from our JME app that are needed to make a goody space
	def getRendRegClient : RenderRegistryClient
	def getWindowStatusMonitor : WindowStatusMonitor
	// def getGMRendCtx : GoodyModularRenderContext
}
trait BodyMgrIngred {
	def	getPMRC : PhysicalModularRenderContext
}

case class FullIngredMsgImpl(rendRegCli : RenderRegistryClient, winStatMon : WindowStatusMonitor,
							 pmrc : PhysicalModularRenderContext)
			extends LesserIngred with BodyMgrIngred {
	override def getRendRegClient : RenderRegistryClient = rendRegCli
	override def getWindowStatusMonitor : WindowStatusMonitor = winStatMon
	override def	getPMRC : PhysicalModularRenderContext = pmrc
}

