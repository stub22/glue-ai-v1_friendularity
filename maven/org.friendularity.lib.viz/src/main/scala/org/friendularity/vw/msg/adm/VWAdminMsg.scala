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

package org.friendularity.vw.msg.adm

import java.awt.Image

import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPMsgTeller, CPStrongTeller}
import org.friendularity.vw.msg.cor.VWorldRequest
import org.friendularity.vw.msg.pub.VWorldPublicTellers

// trait VWBodyLifeRq extends VWorldRequest // disjoint from VWBodyRq

trait VWAdminRqMsg extends VWorldRequest with VarargsLogging

case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg

// Receiver can wait to answer until the system is sufficiently ready, e.g. until the VWorld is up.
// However, Sndr may inquire well after the VWorld is up, and then Rcvr should answer right away.
case class VWARM_FindPublicTellers(answerTeller: CPStrongTeller[VWorldPublicTellers]) extends VWAdminRqMsg

case class VWSetupRq_Conf() extends VWorldRequest // Not being sent as of 2016-06-16

case class VWSetSwingCanvasBranding(canvasTitle: String,  canvasIconImage : Image) extends VWAdminRqMsg
