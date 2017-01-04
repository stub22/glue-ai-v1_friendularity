package org.friendularity.vwgoody

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.{VWorldEntity, GoodyActionExtractor}

import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor


import org.cogchar.render.goody.basic.{BasicGoodyCtx, BasicGoodyCtxImpl}

/**
  * This code moved to new file on 12/14/2016.
  * Guts moved to Dispatch***Goodies on 01/02/2017.
  */

class BetterBGC(rrc : RenderRegistryClient, winStatMon : WindowStatusMonitor)
			extends BasicGoodyCtxImpl(rrc, winStatMon) with DispatchOldGoodies with DispatchCopiedTmpGoodies {

	val FLAG_useOldGoodies = false

	override protected def createByAction(ga: GoodyActionExtractor): VWorldEntity = {

		if (FLAG_useOldGoodies) {
			// This should remain exactly equivalent to function as of 2016-12-31, still using the Cogchar Goody-impls.
			createByAction_emulate2014(ga)
		} else {
			// Copied old Cogchar goody-Impls into these FriendU TG_ impls on 2017-01-02
			createByAction_TG_tmpCopies2017(ga)
		}
	}

}
/*  Here is what the inherited consumeAction method looks like as of Summer 2016.

	override def consumeAction(actionSpec: ThingActionSpec): ConsumpStatus = {
		getLogger.info("The targetThingType is {}", actionSpec.getTargetThingTypeID)
		val ga: GoodyActionExtractor = new GoodyActionExtractor(actionSpec)
		val gid: Ident = ga.getGoodyID
		val vwer = getVWER
		var goodyOne: VWorldEntity = vwer.getGoody(gid)
		val kind: GoodyActionExtractor.Kind = ga.getKind
		getLogger.info("The kind of Goody inspected is {}", kind)

		if (kind != null) {
			ga.getKind match {
				case GoodyActionExtractor.Kind.CREATE => {
					if (vwer.hasGoodyAt(gid)) {
						getLogger.warn("Goody already created! Ignoring additional creation request for goody: {}", gid)
					}
					else {
						goodyOne = createAndAttachByAction(ga, QueueingStyle.QUEUE_AND_RETURN)
						if (goodyOne != null) {
							vwer.addGoody(goodyOne)
							return ConsumpStatus.USED
						}
					}

				}
				case GoodyActionExtractor.Kind.DELETE => {
					if (!vwer.hasGoodyAt(gid)) {
						myVargler.warn1("Could not delete goody because it does not exist: {}", gid)
					}
					else {
						vwer.removeGoody(goodyOne)
						return ConsumpStatus.USED
					}

				}
				case _ => {
					try {
						goodyOne.applyAction(ga, QueueingStyle.QUEUE_AND_RETURN)
						return ConsumpStatus.USED
					}
					catch {
						case e: Exception => {
							myVargler.warn2("Problem attempting to update goody with URI: {}", gid, e)
						}
					}
				}
			}
		}
		return ConsumpStatus.IGNORED
	}
*/
