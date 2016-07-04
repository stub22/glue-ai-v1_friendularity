package org.friendularity.vwimpl

import java.util.concurrent.{Callable => ConcurrentCallable, Future}
import org.cogchar.render.sys.registry.RenderRegistryClient

/**
  * Created by Owner on 7/3/2016.
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
