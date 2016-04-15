package org.friendularity.respire

import akka.actor.{ActorLogging, Actor}
import org.appdapter.core.store.Repo
import org.appdapter.fancy.model.ModelClientImpl
import org.appdapter.fancy.rclient.RepoClient
import org.friendularity.cpump.{Greeting, WhoToGreet}
import org.friendularity.respire.MathyGoodyTest._

/**
  * Created by Owner on 4/13/2016.
  */

// Message target/location/context is interpreted starting from some URI (symbolic node) + offset-vector.
// Offset-vector may use space and time dimensions.  Message may contain further symbols which
// imply sub-targeting.

// We want the SweetSpc to start off as empty as possible, then by populated by a sequence
// of messages, starting with

trait SweetSpcMgr {
	//
}
trait CalcTickMsg {
}
trait ReconfMsg {
}
trait UserInputMsg {
	// Could be a screen click/touch, or other spatial or continuous input.
	// Usually not keystrokes, through this API, as those are more likely to be handled by a central controller,
	// possibly showing up here eventually as ReconfMsg commands.
}
trait StreamFrameMsg {
	// One frame of a regular continuous stream of data
}
trait StatusMsg {
}

abstract class SweetSpcActor extends Actor with ActorLogging {
	def receive = {
		case ctm: CalcTickMsg => {
		}
		case rcnf : ReconfMsg => {
		}
		case uim:  UserInputMsg => {
		}
		case odm: StreamFrameMsg => {
		}
		case stsm: StatusMsg => {
		}
	}
	protected def getMgr : SweetSpcMgr
}
class SweetSpcFactory {

}
class Hmm {
	// From MathyGoodyTest
	def testDynaGoodyItemLoad(repo: Repo, repoClient: RepoClient): SweetDynaSpace = {
		val graphQN = "ccrti:math_sheet_60";
		val spaceSpecQN = "hevi:space_01";
		val graphID = repoClient.getDefaultRdfNodeTranslator.makeIdentForQName(graphQN);
		val mathModel = repo.getNamedModel(graphID)
		val mathModelClient = new ModelClientImpl(mathModel)
		val spaceSpecItem = mathModelClient.makeItemForQName(spaceSpecQN);
		info1("Making MathyGoodySpace for space-spec-item: {}", spaceSpecItem)
		val parentDGS = null;
		val dgs = new MathyGoodySpace(parentDGS, -999, graphID, spaceSpecItem.getIdent);
		// This sets the desired size of the space, but does not actually cause the goodies to be created.
		// That happens during update() on the render thread.
		dgs.refreshFromModelClient(mathModelClient)
		info1("Loaded MathyGoodySpc: {}", dgs)
		dgs
	}
}
/*
From BigBalloon... with DynamicGoodyParent {
	def attachDeepDynaSpace(sweetDS: SweetDynaSpace) {
		Does just this
		sweetDS.setParent(this)
		// Attaches to TrialBalloon.myUpdaters :  List<TrialUpdater>
		// which all get callbacks from the app-level JME-doUpdate().
		attachVWorldUpdater(sweetDS);

 From RespirationTest	def initReposLoadMathEval() : SweetDynaSpace
		Boils down to:
		val rspec = Loads spreadsheet repo-spec
		val dfltTestRepo = rspec.getOrMakeRepo();
		val dfltTestRC = rspec.makeRepoClient(dfltTestRepo);
		MathyGoodyTest.testDynaGoodyItemLoad(dfltTestRepo, dfltTestRC)
		...which is shown above in Hmmm

		The data-grid attached in	CCMIO_WorldEstimateRenderModule
		comes from SnapshotMonitor; // extends TrialContent, shows big matrix of numbers+colors

		In MathBalloon that same grid is being inherited from TrialBalloon, shown as TrialContent.

*/
