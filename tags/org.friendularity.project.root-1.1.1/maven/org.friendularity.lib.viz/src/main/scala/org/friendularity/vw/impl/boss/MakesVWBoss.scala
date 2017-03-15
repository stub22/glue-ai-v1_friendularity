package org.friendularity.vw.impl.boss

import akka.actor.ActorRef
import org.friendularity.infra.akact.KnowsAkkaSys
import org.friendularity.infra.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.vw.impl.sys.VWorldActorFactoryFuncs
import org.friendularity.vw.msg.cor.VWorldRequest

/**
  * Created by Owner on 1/22/2017.
  */
trait KnowsVWBoss {
	def getVWBossTeller : CPStrongTeller[VWorldRequest]
}
trait MakesVWBoss extends KnowsVWBoss with KnowsAkkaSys {
	// lazy val akkaSys = getAkkaSys

	def getVWBossActrName = "vworldBoss_818"

	lazy private val myVWBossAR: ActorRef = VWorldActorFactoryFuncs.makeVWorldBoss(getAkkaSys, getVWBossActrName)
	lazy private val myVWBossTeller : CPStrongTeller[VWorldRequest] = new ActorRefCPMsgTeller(myVWBossAR)

	override def getVWBossTeller : CPStrongTeller[VWorldRequest] = myVWBossTeller
}
