package org.friendularity.respire

import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.RepoClient

import org.cogchar.api.owrap.appro.{ASBRNCharacter, ASBRNVWorldSimRuntime, ASBRNEmbedPhysRuntime,  ACRGamePipe}
import org.cogchar.api.thing.WantsThingAction
import org.cogchar.app.puma.behavior.PumaBehaviorManager
import org.cogchar.app.puma.body.PumaDualBodyManager
import org.cogchar.app.puma.boot.{PumaContextCommandBox, PumaSysCtx}
import org.cogchar.app.puma.config.{BodyHandleRecord, PumaContextMediator}
import org.cogchar.app.puma.event.{Updater, CommandEvent}
import org.cogchar.app.puma.registry.{PumaRegistryClientImpl, PumaRegistryClient}
import org.cogchar.bundle.app.vworld.busker.TriggerItems
import org.cogchar.bundle.app.vworld.central.{VWCtxCmdBox, StatefulVWorldRegistry, VWorldRegistry}
import org.cogchar.platform.trigger.BoxSpace;
import org.cogchar.platform.trigger.CommandSpace
import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.cogchar.render.sys.goody.{GoodyModularRenderContext, GoodyRenderRegistryClient}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor


import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent
import org.osgi.framework.BundleContext



import java.util.{Map => JMap, ArrayList => JArrayList}
/** README
  * As of June 2016, none of this VWChef.scala code is used in CCMIO demo.
  * However, it provides important reference points and documented design info.
  * ---------------------------------------------------------------------------
  *
  * Chef uses modern recipes and fresh chunks, to prepare tasty delights!
  *
  * Chef is responsible for preparing the state of the runtime, using the
  * same ingredients (config-records translated to config-messages) that any other client
  * can also send programmatically (as RDF, Java-serialized message, HTTP/JSON,
  * QPid/Avro, etc).  Nothing the Chef does is magic; it's just hard work that
  * Chef is good at doing quickly.
  *
  * Chef likes 3 kinds of chunks:
  *
  * grump - and other character sets, which set up motion, speech, and intrinsic char behavior.
  * vwon - and other vworld-conf sets, which set up the vworld canvas, lighting, cameras, facilities
  * funpak - and other goody-conf/game/content+rule sets
  *
  * It is not Chef's job to select/load the appro-profile from disk, that happens upstream of her.
  * The profile she receives contains all the recipes she needs.  Using those she is able to scan
  * the 3 kinds of chunks listed above, looking for relevant config/content data graphs and URIs.
  */

class VWChef {
	def prepareDishes(mergedProfJM : JenaModel) : Unit = {
		// Starting with:
		// 0A) existing profile recipes already loaded, and
		// 0B) An appropriate way to get eHosts for the given platform, which may use profile #0A as much
		// as appropriate, as well as injected services.  This part is suitable
		// for lifecycle/registry integration, especially the eHost connection.  Also works
		// OK from regular classpath, when OSGi is not present.

		// Do:
		// 1) Find recipes for nexuses and circuses (nexi y circi = crosses and circles).
		// Each of these corresponds to a channel-actor that we must find or create
		// (steps 2 + 3), then pump up with initial population in step 4.

		// 2) Get all the connected feature-broker config recipes.

		// 3) Load/find the corresponding chunks, from appropriate eHosts.  (See #0B above).

		// 4) Send appropriate population messages to appropriate channel-actors, creating those
		// actors when needed.

		// 5) Mark actors with appropriate recipe URIs (or related value-data) so they can be found by clients,
		// either/both implicitly using  lower level Actor paths, or more explicitly as tellers known to CPump sys.
		// The latter is preferred.
	}
}
trait LesserIngred {
	// Includes pointers from our JME app that are needed to make a goody space
	def getRendRegClient : RenderRegistryClient
	def getWindowStatusMonitor : WindowStatusMonitor
	// def getGMRendCtx : GoodyModularRenderContext
}
trait BodyMgrIngred {
	def	getPMRC : PhysicalModularRenderContext
}
case class FullIngredImpl(rendRegCli : RenderRegistryClient, winStatMon : WindowStatusMonitor, pmrc : PhysicalModularRenderContext)
		extends LesserIngred with BodyMgrIngred {
	override def getRendRegClient : RenderRegistryClient = rendRegCli
	override def getWindowStatusMonitor : WindowStatusMonitor = winStatMon
	override def	getPMRC : PhysicalModularRenderContext = pmrc
}




