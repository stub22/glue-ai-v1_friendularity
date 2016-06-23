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

package org.friendularity.respire

import akka.actor.{ActorLogging, Actor}
import com.jme3.font.BitmapText
import com.jme3.material.Material
import com.jme3.math.{Vector3f, FastMath, Quaternion, ColorRGBA}
import com.jme3.renderer.Camera
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.shape.Quad
import com.jme3.scene.{Spatial, Geometry, Mesh, Node => JmeNode}
import org.appdapter.core.name.Ident
import org.appdapter.core.store.Repo
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.model.ModelClientImpl
import org.appdapter.fancy.rclient.RepoClient
import org.cogchar.api.space._
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.{CameraBinding, GoodyActionExtractor, VWorldEntity}
import org.cogchar.render.goody.basic.{GoodyBox, VirtualFloor}
import org.cogchar.render.goody.bit.{TicTacGrid, TicTacMark, BitCube, BitBox}
import org.cogchar.render.goody.flat.{ParagraphGoody, ScoreBoardGoody, CrossHairGoody}
import org.cogchar.render.optic.goody.VWorldCameraEntity
import org.cogchar.render.sys.goody.GoodyRenderRegistryClient
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialNexus, TextSpatialFactory}
import org.friendularity.cpump.{Greeting, WhoToGreet}
import org.friendularity.vwimpl.{Brush, OuterGuy}


/**
  * Created by Owner on 4/13/2016.
  */

// Message target/location/context is interpreted starting from some URI (symbolic node) + offset-vector.
// Offset-vector may use space and time dimensions.  Message may contain further symbols which
// imply sub-targeting.

// We want the SweetSpc to start off as empty as possible, then by populated by a sequence
// of async messages.

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
class Hmm extends VarargsLogging {
	// From MathyGoodyTest
	def testLoadMathyGoodySpace(repo: Repo, repoClient: RepoClient): SweetDynaSpace = {
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
		comes from SnapshotMonitor (also in CCMIO); extends TrialContent, shows big matrix of numbers+colors

		In MathBalloon that same grid is being inherited from TrialBalloon, shown as TrialContent.

*/

trait DetachedGST extends VarargsLogging {
	// Copied from org.cogchar.api.space.GridSpaceTest
	def gridSpaceTest : Unit = {
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  Detached standalone GridSpace test, does not talk to VWorld")
		// This block from x=3,y=-1 to x=5,y=6 extends "beyond" its implied containing cell space, which starts at x=1,y=1
		val cellBlock = CellRangeFactory.makeBlock2D(3, 5, -1, 6)
		info1("DetachedGST sez:  CellBlock description={}", cellBlock.describe(1)) // cellFrom == 1 -> base-1 labelling

		val space2D : MultiDimGridSpace = GridSpaceFactory.makeSpace2D(5, 80.0f, 120.0f, 7, -20.0f, 15.0f)
		info1("DetachedGST sez:  2D Space description={}", space2D.describe()) // cellFrom == 1 -> base-1 labelling

		val posBlock = space2D.computePosBlockForCellBlock(cellBlock);
		info1("DetachedGST sez:  Computed result PosBlock description={}", posBlock.describe)
		val vecOnDiag = posBlock.getVecFromMainDiagonal(2.0f)
		info1("DetachedGST sez:  Vec on pos-block diag at 2.0f * MAX ={}", vecOnDiag)

		val vecAtMin = posBlock.getVecFromMainDiagonal(0.0f)
		info1("DetachedGST sez:  Vec on pos-block diag at 0.0f * MAX ={}", vecAtMin)

		val blockAt729 = CellRangeFactory.makeUnitBlock3D(7, 2, 9)
		info1("DetachedGST sez:  3D unit block at 7,2,9 description={}", blockAt729.describe(1))

		val space3D : MultiDimGridSpace = GridSpaceFactory.makeSpace3D(7, -40.0f, 40.0f, 5, -20.0f, 20.0f, 9, -50.0f, 20.0f);
		info1("DetachedGST sez:  3D Space description={}", space2D.describe()) // cellFrom == 1 -> base-1 labelling

		info0("DetachedGST sez:  Detached Grid Space Test - COMPLETE")

	}

}
import TrialNexus.BlendedShapeContext



//  Captures TrialNexus content, not used from lib.viz, yet (we still get original TN).
trait Srtw extends VarargsLogging {
	def getRRC : RenderRegistryClient
	lazy val rrcli = getRRC
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(rrcli)

	def getOuterGuy : OuterGuy

	def makeFunSubspace(deepSpace : MultiDimGridSpace): Unit = {
		// Make integer index space range, where 6 args are 3 pairs for x,y,z: (firstX, lastX), (firstY...
		val extrudedCellBlock: CellBlock = CellRangeFactory.makeBlock3D(3, 5, -1, 6, 2, 7)
		val extrudedPosBlock: PosBlock = deepSpace.computePosBlockForCellBlock(extrudedCellBlock)
		getLogger.info("Computed result PosBlock description={}", extrudedPosBlock.describe)

		val xIndexBlock =  extrudedCellBlock.myCIRs(0)
		val chosenXPos : Range = xIndexBlock.getFirstCellIndex.getIndexFrom0 to xIndexBlock.getLastCellIndex.getIndexFrom0
		info1("xPosRange using 0-idx is: {}", chosenXPos)
	}

	def makeSymmetricCenteredGridSpace (countX : Int, widthX : Float, countY : Int, heightY : Float,
										countZ : Int, depthZ : Float) : MultiDimGridSpace = {
		GridSpaceFactory.makeSpace3D(countX, widthX / -2.0f, widthX/2.0f, countY, heightY/ -2.0f, heightY/2.0f,
			countZ, depthZ/ -2.0f, depthZ/2.0f)
	}
	def makeUniformCenteredGridSpace(countPerDim : Int, lengthPerDim : Float) : MultiDimGridSpace = {
		makeSymmetricCenteredGridSpace(countPerDim, lengthPerDim, countPerDim, lengthPerDim, countPerDim, lengthPerDim )
	}
	// From org.cogchar.render.trial.TrialNexus
	def makeSheetspace(uniformCount : Int, uniformLen : Float, chosenIdxs_X: Range, chosenIdxs_Y : Range,
					   chosenIdxs_Z : Range) : JmeNode = {
		info0("Begin Srtw.makeSheetspace")

		// Defines an efficient grid space, which does not allocate objects for cells.
		// It has infinite implied structure, e.g. cell(-99999, 5555555, -44444444) is well defined.
		val deepSpace: MultiDimGridSpace = makeUniformCenteredGridSpace(uniformCount, uniformLen)

		getLogger.info("Uniform space description={}", deepSpace.describe)

		// Make and attach wig nodes for each combination of chosen x,y,z indices
		val wigsParent : JmeNode = makeWigs(deepSpace, chosenIdxs_X, chosenIdxs_Y, chosenIdxs_Z)

		val vizNode: JmeNode = new JmeNode("sspace_viz_node_" + System.currentTimeMillis())

		vizNode.attachChild(wigsParent)
		vizNode
	}
	def makeWigs(deepSpace: MultiDimGridSpace, chosenIdxs_X: Range, chosenIdxs_Y: Range, chosenIdxs_Z: Range) : JmeNode = {
		val outerGuy : OuterGuy = getOuterGuy
		val quadMeshFiveByFive: Mesh = new Quad(5, 5)
		val wsParentNode: JmeNode = new JmeNode("wsParent_" + System.currentTimeMillis())

		var seq: Int = 0
		for (zIdx <- chosenIdxs_Z)
			for (yIdx <- chosenIdxs_Y)
				for (xIdx <- chosenIdxs_X) {
					// Start by making a unit-cell-block for the given index combination.
					// (Alternatively, this key block could contain a range of cells)
					val unitCB: CellBlock = CellRangeFactory.makeUnitBlock3D(xIdx, yIdx, zIdx)
					// Find the physical floating-point boundaries of that given cell unit/group.
					val unitPB: PosBlock = deepSpace.computePosBlockForCellBlock(unitCB)
					debug3("Unit cell with seq#={} has cellBlock={} and posBlock.description={}", seq : Integer, unitCB, unitPB.describe)

					val wigNode : JmeNode = makeBoxWig(unitPB,
								outerGuy.myBrushJar.cyan_med,  outerGuy.myBrushJar.orange_med,
								quadMeshFiveByFive, seq)
					val wigNum = wsParentNode.attachChild(wigNode)
					if (seq % 23 == 0) {
						info3("Made wigNum={} at seq={}, wig={}", wigNum : Integer, seq : Integer, wigNode)
					}

					seq += 1
				}

		info0("End Srtw.makeSheetspace")
		wsParentNode
	}
	def translateToMins(spat : Spatial, unitPB: PosBlock) : Unit = {
		val xpr: PosRange = unitPB.myPRs(0)
		val ypr: PosRange = unitPB.myPRs(1)
		val zpr: PosRange = unitPB.myPRs(2)
		spat.setLocalTranslation(xpr.getMin, ypr.getMin, zpr.getMin)
	}

	def makeBoxWig(unitPB: PosBlock,  frontBrush : Brush, sideBrush : Brush, qMesh : Mesh,
				   seq : Int) : JmeNode = { // xi : Int, yi: Int, zi: Int)

		val wigNode = new JmeNode("wig_" + seq)


		val qlabTxt01: String = "shc_" + seq + "_1"
		val qg1: Geometry = new Geometry(qlabTxt01, qMesh)
		frontBrush.stroke(qg1)  // TrialNexus uses bsc1 here
		val g = -0.02f
		val pos = 0.4f
		val mum = 0.7f
		qg1.setLocalTranslation(g, g, g) // xpr.getMin, ypr.getMin, zpr.getMin)
		wigNode.attachChild(qg1)

		val outerGuy = getOuterGuy

		val someBT : BitmapText = outerGuy.happyTxtMaker.makeBitmapTxt(qlabTxt01)
		val qlabBT_01: BitmapText = someBT //  myFirstTSF.makeTextSpatial(qlabTxt01, 0.2f, RenderQueue.Bucket.Transparent, 6)

		qlabBT_01.setLocalTranslation(pos, pos, pos) //) xpr.getCenter, ypr.getCenter, zpr.getCenter)
		wigNode.attachChild(qlabBT_01)

		val dmaker = new DoodleMaker(sideBrush.myBSC, qMesh) // TrialNexus uses bsc2 here

		val doodle : Geometry = dmaker.makeDoodle(seq, mum, mum, mum) // xpr.getMax, ypr.getCenter, zpr.getCenter)
		val whatIdx : Int = wigNode.attachChild(doodle)
		debug2("Doodle for seq={} attached at child={}", seq : Integer, whatIdx : Integer)
		translateToMins(wigNode, unitPB)

		val wigRot = new Quaternion().fromAngleAxis(FastMath.HALF_PI * 0.01f * seq, Vector3f.UNIT_Z)
		wigNode.setLocalRotation(wigRot)
		wigNode
	}
	def makePlaceAndAttach() = ???

}
class DoodleMaker(bsc : BlendedShapeContext, qMesh : Mesh) {
	lazy val myRot = new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_Y)
	def getRot : Quaternion = myRot
	// Reaturns a geom which is a spat that may be added as child of a node
	def makeDoodle(seq: Int, xf : Float, yf : Float, zf : Float) : Geometry = {
		val qlabTxt02: String = "dood_" + seq + "_2"
		val qg2: Geometry = new Geometry(qlabTxt02, qMesh)
		bsc.setupGeom(qg2)
		val spat : Spatial = qg2
		spat.setLocalTranslation(xf, yf, zf)
		val rotAboutY_90: Quaternion = getRot
		spat.setLocalRotation(rotAboutY_90)
		qg2
	}
}
