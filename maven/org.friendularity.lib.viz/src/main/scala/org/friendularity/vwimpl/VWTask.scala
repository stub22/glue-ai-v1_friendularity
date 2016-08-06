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

import java.util.concurrent.{Callable => ConcurrentCallable, Future}

import com.jme3.scene.Spatial.CullHint
import com.jme3.scene.{Node => JmeNode, Spatial}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.field.{SourceDataMsg, ReportingPolicy, SchedTaskRepeating, ScheduleHelper}
import org.friendularity.vwmsg.VWExoBodyChance

/**
  * Created by Stub22 on 7/3/2016.
  */
trait VWTask

trait EnqHlp {
	def makeJConcurCallable(func: Function0[Unit]) : ConcurrentCallable[Unit] = {
		new ConcurrentCallable[Unit] {
			override def call: Unit = {
				func()
			}
		}
	}
	def enqueueJmeCallable(rrc : RenderRegistryClient, func: Function0[Unit]) : Future[Unit] = {
		val workAppStub = rrc.getWorkaroundAppStub
		val callable = makeJConcurCallable(func)
		workAppStub.enqueue(callable)
	}
}
trait FullEnqHlp extends EnqHlp {
	protected def getRRC : RenderRegistryClient
	def enqueueJmeCallable(func: Function0[Unit]): Unit = {
		enqueueJmeCallable(getRRC, func)
	}
}

trait AttachHlp extends FullEnqHlp {
	def enqueueAttach(childSpat : Spatial, parentNode : JmeNode) : Unit = {
		val deferredAttachFunc : Function0[Unit] = () => {parentNode.attachChild(childSpat)}
		enqueueJmeCallable(deferredAttachFunc)
	}
	def enqueueDetach(childSpat : Spatial) : Unit = {
		val deferredDetachFunc : Function0[Unit] = () => {childSpat.removeFromParent()}
		enqueueJmeCallable(deferredDetachFunc)
	}

	def enqueueCullHint(spat : Spatial, ch : CullHint) : Unit = {
		val deferredFunc : Function0[Unit] = () => {spat.setCullHint(ch)}
		enqueueJmeCallable(deferredFunc)
	}
}
trait SvcGate extends AttachHlp {

}


