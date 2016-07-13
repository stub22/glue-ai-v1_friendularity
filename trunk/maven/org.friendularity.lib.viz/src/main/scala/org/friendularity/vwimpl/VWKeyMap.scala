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
package org.friendularity.vwimpl

import com.jme3.input.{FlyByCamera, InputManager}
import com.jme3.input.controls.{Trigger, ActionListener}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.platform.gui.keybind.KeyBindingTracker
import org.cogchar.render.sys.input.VW_InputBindingFuncs
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.vwmsg.VWorldPublicTellers

/**
  * Created by Stub22 on 6/26/2016.
  */
trait VWKeyMapLogic extends VarargsLogging {
	private def makeJmeActionListener(vwpt: VWorldPublicTellers,
									  actionsByName : Map[String,Function1[VWorldPublicTellers,Unit]])
	= new ActionListener {
		override def onAction(actionName: String, isPressed: Boolean, tpf: Float): Unit = {
			if (isPressed) {
				val actFunc_opt = actionsByName.get(actionName)
				info2("VWStage Listener for JME:  Resolved actionName={} to func_opt={}", actionName, actFunc_opt)
				if (actFunc_opt.isDefined) {
					actFunc_opt.get.apply(vwpt)
				}
			} else {
				info1("Ignoring un-pressed event at action={}", actionName)
			}
		}
	}
	// Uses additive semantics.  If called multiple times, may result in duplicate or overlapping bindings.
	// To start over, use clearJmeInputMappings... below.
	// All state is kept in the JME mappings + listener bindings.
	def registerKeymap(rrc: RenderRegistryClient, keynamesToActions : Map[String,Function1[VWorldPublicTellers,Unit]],
					   pubTellers : VWorldPublicTellers) : Unit = {
		val jmeInpMgr : InputManager = rrc.getJme3InputManager(null)

		val baloney = new KeyBindingTracker // We don't actually use this, but is wanted by makeJME3InputTriggers

		var theseActionsByName : Map[String,Function1[VWorldPublicTellers,Unit]] = Map()
		for ((keyOrInputName,actFunc) <- keynamesToActions) {
			// 3 steps: 1) create/find a JME trigger, 2) register the trigger to an actionName, and 3) assoc the cback.
			// val actionNameFromChr = "actionFor_" + keyChr
			val ourKeyConstant = VW_InputBindingFuncs.getKeyConstantForName(keyOrInputName)
			if (ourKeyConstant != VW_InputBindingFuncs.NULL_KEY) {
				val actionName = "actFor_" + keyOrInputName
				val triggersArr : Array[Trigger] = VW_InputBindingFuncs.makeJME3InputTriggers(ourKeyConstant, actionName, baloney)
				if (triggersArr.length == 1) {
					info3("Registering triggers={} for action={} at inputCode={}", triggersArr, actionName, ourKeyConstant : Integer)
					jmeInpMgr.addMapping(actionName, triggersArr(0))
				}  else {
					warn3("Got wrong number ot triggers={} for action={} at inputCode={}", triggersArr, actionName, ourKeyConstant : Integer)
				}
				theseActionsByName += (actionName -> actFunc)
			}
		}
		val theseActionNames : Array[String] = theseActionsByName.keys.toArray

		val listener = makeJmeActionListener(pubTellers, theseActionsByName)
		jmeInpMgr.addListener(listener, theseActionNames : _*) // cast to java method varargs

	}

	protected def clearJmeInputMappingsAndRelinkFBCam(rrc: RenderRegistryClient) : Unit = {
		val jmeInpMgr : InputManager = rrc.getJme3InputManager(null)
		jmeInpMgr.clearMappings;
		val workAppStub = rrc.getWorkaroundAppStub
		// Since we just cleared mappings and are (for now at least) using the default FlyByCamera mappings, we must re-register them
		val fbCam: FlyByCamera = workAppStub.getFlyByCamera
		fbCam.registerWithInput(jmeInpMgr)

		// val appSett : AppSettings = rrc.get  - but we have only *set*

	}
}
