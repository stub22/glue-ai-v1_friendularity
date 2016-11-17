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
package org.friendularity.navui

import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.akact.KnowsAkkaSys
import org.friendularity.closing.SpeechServerCloser
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPMsgTeller, CPStrongTeller, CPumpMsg}
import org.friendularity.cpump.ScheduleHelper
import org.friendularity.dull.SpecialAppPumpSpace
import org.friendularity.vwimpl.LegacyBodyLoader_Stateless
import org.friendularity.vwmsg.{VWARM_GreetFromPumpAdmin, VWBodyNotice}
import org.osgi.framework.BundleContext

/**
  * Created by Stub22 on 6/8/2016.
  * // "App" here means FriendU app, not a JME3 "app".  (This instance is several layers further out)
  * // The latter is made during launchSimRenderSpace in VWCore.scala.
  */
trait NavUiAppSvc extends VarargsLogging {

  def postPatientCharCreateRq(dualBodyID: Ident, fullHumaCfg: HumanoidFigureConfig,
                              mbrsc_opt: Option[ModelBlendingRobotServiceContext],
                              answerTeller: CPStrongTeller[VWBodyNotice]): Unit

  def makeExoBodyUserTeller_withTicks(akkaSys: ActorSystem, ebuActorName: String, userLogic: ExoBodyUserLogic): CPStrongTeller[VWBodyNotice] = {
    val parentARF: ActorRefFactory = akkaSys
    val ebuActor: ActorRef = ExoActorFactory.makeExoBodyUserActor(parentARF, ebuActorName, userLogic)
    val ebuTeller: CPStrongTeller[VWBodyNotice] = new ActorRefCPMsgTeller[VWBodyNotice](ebuActor)
    // Let the user logic create the TickItem = callback func and schedule params
    val regTickItem = userLogic.makeRegularTickItem()
    regTickItem.addToSchedForSys(akkaSys, ebuActor, ebuActor)
    ebuTeller
  }

  def UNUSED_scheduleCallback_UNUSED(akkaSys: ActorSystem, tgtActor: ActorRef, tickMsg: CPumpMsg, phaseMillis: Integer,
                                     periodMillis: Integer, schedHelper: ScheduleHelper): Unit = {
    val schedItem = schedHelper.makeSchedItemRepeating(tickMsg, phaseMillis, periodMillis)
    schedItem.addToSchedForSys(akkaSys, tgtActor, tgtActor)
  }

  def makeFunUserLogic(flag_sendTestMoves: Boolean): ExoBodyUserLogic = {
    val btc = new BodyTestClient {}
    val userLogic: ExoBodyUserLogic = if (flag_sendTestMoves)
      btc.makeMoveTestLogic
    else
      btc.makeEmptyExoBodyUserLogic
    userLogic
  }

  val sinbadBodyID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#char_sinbad_88")
  val sinbadHmdGraphID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#hmd_sheet_22")
  val sinbadBonyGraphID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#bony_sheet_sinbad")

  // Includes MechIO anim connections, requires bundleCtx.
  // TODO:  If we were sending to an actor that knew how to discover the MechIOBody connection,
  // possibly by waiting for a lifecycle update, then this impl could be same as the "Standy" method below.
  def requestSemiLegacyBodyConn_OSGi_Sinbad(bundleCtx: BundleContext, akkaSys: ActorSystem,
                                            legacyELRC: EnhancedLocalRepoClient, exoBodyUserLogic: ExoBodyUserLogic): Unit = {
    requestSemiLegacyBodyConn_OSGi(bundleCtx, akkaSys, legacyELRC, sinbadBodyID,
      sinbadHmdGraphID, sinbadBonyGraphID, exoBodyUserLogic)
  }

  def requestSemiLegacyBodyConn_OSGi(bundleCtx: BundleContext, akkaSys: ActorSystem,
                                     legacyELRC: EnhancedLocalRepoClient, charBodyID: Ident,
                                     hmdGraphID: Ident, bonyGraphID: Ident,
                                     exoBodyUserLogic: ExoBodyUserLogic): Unit = {

    val legBodyLdr = new LegacyBodyLoader_Stateless
    val fullHumaCfg: HumanoidFigureConfig = legBodyLdr.loadFullHumaConfig_SemiLegacy(legacyELRC, charBodyID, hmdGraphID, bonyGraphID)
    val mbrsc: ModelBlendingRobotServiceContext = legBodyLdr.connectMechIOBody(legacyELRC, bundleCtx, fullHumaCfg, bonyGraphID)

    // Before we create the body itself, create an external client actor, with ability to
    // talk to VW body, and also to receive schedule ticks
    val noticerName = "bodyNoticer_" + charBodyID.getLocalName
    val bodyNoticer: CPStrongTeller[VWBodyNotice] = makeExoBodyUserTeller_withTicks(akkaSys, noticerName, exoBodyUserLogic)

    // Now we've done all the "outer" setup that requires assumptions, and we can
    // send off a tidy async request to the v-world actors, requesting them to
    // instantiate the avatar body and send back a notice when done, to our bodyNoticer.
    // THEN our bodyNoticer can send more requests do any additional manipulation on the body
    // such as move its v-world position and orientation, attach a camera, launch an animation.
    // HOWEVER, it is also possible to indepdently find bodies, as is done by VWThingActReqRouterLogic
    // using VWBodyTARouterLogic and VWBodyMedialRendezvous.
    postPatientCharCreateRq(charBodyID, fullHumaCfg, Option(mbrsc), bodyNoticer)

  }

  // Creates a posable VW character, but does not ask for or assume any MechIO (or other OSGi) infrastructure.
  def requestStandySemiLegacyBody_Sinbad(akkaSys: ActorSystem,
                                         legacyELRC: EnhancedLocalRepoClient,
                                         exoBodyUserLogic: ExoBodyUserLogic): Unit = {
    val legBodyLdr = new LegacyBodyLoader_Stateless
    val fullHumaCfg: HumanoidFigureConfig = legBodyLdr.loadFullHumaConfig_SemiLegacy(legacyELRC, sinbadBodyID, sinbadHmdGraphID, sinbadBonyGraphID)

    // Before we create the body itself, create an external client actor, with ability to
    // talk to VW body, and also to receive schedule ticks
    val bodyNoticer: CPStrongTeller[VWBodyNotice] = makeExoBodyUserTeller_withTicks(akkaSys, "sinbad_standy_body_user", exoBodyUserLogic)
    postPatientCharCreateRq(sinbadBodyID, fullHumaCfg, None, bodyNoticer)

  }
}

trait NavAppCloser extends VarargsLogging {
  protected def getAkkaSys: ActorSystem

  // TODO:  Initiate this shutdown via an actor msg
  def closeTheApp: Unit = {
    val speechServerCloser: SpeechServerCloser = new SpeechServerCloser()
    warn1("closeTheApp is calling close on speechServerCloser={}", speechServerCloser)
    speechServerCloser.close

    val akkaSys = getAkkaSys
    warn1("closeTheApp is calling shutdown on akkaSys={}", akkaSys)
    akkaSys.shutdown()

    // TODO:  Shutdown QPid broker (Possibly sending higher level "Goodbye" to partners, first)
    // TODO:  Stop Body-anim modules
    // TODO:  Close windows
    // TODO:  Close AL devices
    // TODO:  Close MIDI devices
    // TODO:  Stop OSGi bundles
    // TODO:  Become compatible with robot shutdown

    Thread.sleep(600)
    System.exit(0)

  }

}

trait NavPumpSpaceOwner extends KnowsAkkaSys with VarargsLogging {

  lazy private val myPumpSpace = new SpecialAppPumpSpace(getAkkaSys)
  lazy private val standPumpTestCtxName = NavUiTestPublicNames.cpumpName
  lazy private val standPumpCtxActorRef: ActorRef = myPumpSpace.findTopActorRef(standPumpTestCtxName)
  lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

  protected def sendGreetMsgs_Async(vwBossTeller: CPMsgTeller): Unit = {
    // We send a currently-non-essential administrative howdy to get the game rollin
    val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
    info2("Sending greeting msg={} to VWBossTeller : {}", hpatMsg, vwBossTeller)
    vwBossTeller.tellCPMsg(hpatMsg)
  }

}

