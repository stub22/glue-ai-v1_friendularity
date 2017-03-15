package org.friendularity.vw.impl.sys

import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.vw.msg.pub.VWorldPublicTellers


/**
  * Code moved to new file on 1/19/2017.
  */
trait VWPTRendezvous {
	private var myListeners : List[CPStrongTeller[VWorldPublicTellers]] = Nil
	private var myVWPT_opt : Option[VWorldPublicTellers] = None

	def addVWPTListener (teller: CPStrongTeller[VWorldPublicTellers]) : Unit = {
		synchronized{
			myListeners = teller :: myListeners
			if (myVWPT_opt.isDefined) {
				teller.tellStrongCPMsg(myVWPT_opt.get)
			}
		}
	}
	protected def notifyVWPTListeners(vwpt : VWorldPublicTellers) : Unit = {
		synchronized {
			myListeners.foreach (_.tellStrongCPMsg (vwpt) )
		}
	}
	def setVWPT(vwpt : VWorldPublicTellers): Unit = {
		synchronized {
			myVWPT_opt = Option (vwpt)
			notifyVWPTListeners (vwpt)
		}
	}
}
