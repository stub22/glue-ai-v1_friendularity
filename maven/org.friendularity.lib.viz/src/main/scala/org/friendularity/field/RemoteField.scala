package org.friendularity.field

import scala.collection.mutable.{HashMap => MutableHashMap}
/**
  * Created by Owner on 7/10/2016.
  */
// RemoteItem = A piece of data that we may read+write as a client of some remote service (e.g. some actor).
// Lower level data-port concept, where we *do* param by CType.
trait RemoteItemField[CType] {
	def getItemFieldSpec : ItemFieldSpec
}
trait SendableRemoteItemField[CType] extends RemoteItemField[CType] {
	def sendContent(c : CType) : Unit
}
trait ReceivableRemoteItemField[CType] extends RemoteItemField[CType]  {
	// def notifyContentUpdate()
	var myChangeHandlers : List[Function1[CType,Unit]] = Nil
	def regChangeHandler(func : Function1[CType,Unit]) : Unit = {
		myChangeHandlers = func :: myChangeHandlers
	}
	def notifyDataChange(upDat : CType) : Unit = {
		// Invokes handlers in this thread
		myChangeHandlers.map(_.apply(upDat))
	}
}




/*
// Interesting choice here of whether to parametrize this type.  Currently punting with a Wildcard.
trait RegisterItemFieldInterestMsg extends ItemFieldDataMsg {
	def getNoticeTeler : CPStrongTeller[ItemFieldDataMsg] // ItemFieldDataChgMsg[_]]  // Note wildcard.
*/
/*
case class RegisterForDataChgNotices(interestRegID : Ident, myItemFieldSpec : ItemFieldSpec,
									 myNoticeTeller : CPStrongTeller[ItemFieldDataChgMsg[_]],
									 updatePeriodSec : Float)
			extends  ItemInterestRegMsg {
//	override def getFieldSpec: ItemFieldSpec = myItemFieldSpec
//	override def getNoticeTeler: CPStrongTeller[ItemFieldDataChgMsg[_]] = myNoticeTeller

	override def getUpdatePeriodSec : Float = updatePeriodSec
}
*/
// For these data-items, we can presume the serializable message is same in both directions.
/*
case class RemoteItemFieldSenderImpl[CType](myItemFieldSpec : ItemFieldSpec,
											myRemoteTeller : CPStrongTeller[ItemFieldDataChgMsg[CType]])
			extends SendableRemoteItemField[CType] {
	override def sendContent(c: CType): Unit = {
		val msg = new ItemFieldDataChgMsgImpl[CType](myItemFieldSpec, c)
		myRemoteTeller.tellStrongCPMsg(msg)
	}
	override def getItemFieldSpec: ItemFieldSpec = myItemFieldSpec
}
case class RemoteItemFieldReceiverImpl[CType](myItemFieldSpec : ItemFieldSpec,
											  myRemoteTeller : CPStrongTeller[RegisterItemFieldInterestMsg],
											  myLocalNoticeTeller : CPStrongTeller[ItemFieldDataChgMsg[_]]		 )
			extends ReceivableRemoteItemField[CType] {
	override def getItemFieldSpec: ItemFieldSpec = myItemFieldSpec

	def registerForNotices(updatePeriodSec : Float): Unit = {
		val registerMeMsg = new RegisterForDataChgNotices(myItemFieldSpec, myLocalNoticeTeller, updatePeriodSec)
		myRemoteTeller.tellStrongCPMsg (registerMeMsg)
		// But how does myLocalNoticeTeller know about us?
	}
}
*/
// Either all the callback-closures must be made before this actor is constructed,
// OR, we must be able to send in some kind of closure, or some other actor.

/*
trait InterestRegMsg extends CPumpMsg {
	// The groupID is used later to activate/deactivate/suspend/modify a set of one or more existing registrations.
	def getRegGroupID_opt : Option[Ident] = None // If not supplied, then will be auto-assigned to a new group,
	// which client will have a harder time discovering.
}
*/

/*
class DuplexRemoteItemFieldImpl[CType](myItemFieldSpec : ItemFieldSpec,
								  myRemoteTeller : CPStrongTeller[ItemFieldDataMsg])
			extends SendableRemoteItemField[CType] with ReceivableRemoteItemField[CType] {

	override def getItemFieldSpec: ItemFieldSpec = myItemFieldSpec

	lazy val mySender = new RemoteItemFieldSenderImpl[CType](myItemFieldSpec, myRemoteTeller)
	lazy val myRcvr = new RemoteItemFieldReceiverImpl(myItemFieldSpec, myRemoteTeller)

	override def sendContent(c: CType): Unit = mySender.sendContent(c)

	def registerForChangeNotices(remoteTeller : CPMsgTeller, callbackTeller : CPMsgTeller) : Unit = {
		val registerMeMsg = new RegisterForDataChgNotices(getItemID, getFieldID, callbackTeller)
		myRemoteTeller.tellStrongCPMsg(registerMeMsg)
	}
}
*/
