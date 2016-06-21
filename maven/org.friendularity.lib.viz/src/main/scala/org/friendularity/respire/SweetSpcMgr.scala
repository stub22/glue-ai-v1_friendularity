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
import com.jme3.scene.{Spatial, Geometry, Mesh, Node}
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

class OuterGuy(myRRC : RenderRegistryClient, myMatPal : MatPallete) {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(myRRC)
	val myBrushJar = new BrushJar(myMatPal)

	val quadMeshFiveByFive: Mesh = new Quad(5,5)

	val redQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)


}

//  Captures TrialNexus content, not used from lib.viz, yet (we still get original TN).
trait Srtw extends VarargsLogging {
	def getRRC : RenderRegistryClient
	lazy val rrcli = getRRC
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(rrcli)

	def getOuterGuy : OuterGuy

	// From org.cogchar.render.trial.TrialNexus
	def makeSheetspace(parentNode: Node) {
		info0("Begin Srtw.makeSheetspace")
		val xCount: Int = 7
		val yCount: Int = 5
		val zCount: Int = 9
		val deepSpace: MultiDimGridSpace = GridSpaceFactory.makeSpace3D(xCount, -120.0f, 120.0f, yCount, -60.0f, 60.0f, zCount, -200.0f, 100.0f)
		getLogger.info("Space description={}", deepSpace.describe)
		// Make integer index space range, where 6 args are 3 pairs for x,y,z: (firstX, lastX), (firstY...
		val extrudedCellBlock: CellBlock = CellRangeFactory.makeBlock3D(3, 5, -1, 6, 2, 7)
		val extrudedPosBlock: PosBlock = deepSpace.computePosBlockForCellBlock(extrudedCellBlock)
		getLogger.info("Computed result PosBlock description={}", extrudedPosBlock.describe)

		val vizNode: Node = new Node("sspace_viz_node_" + System.currentTimeMillis())
		parentNode.attachChild(vizNode)


		val cellCount: Int = xCount * yCount * zCount
		var seq: Int = 0
		val quadMeshFiveByFive: Mesh = new Quad(5,5)

		val outerGuy = getOuterGuy

		// Original does loops over x, y, z, attaching instances.  Here so far
		// we show just the creation + attachment of one group of instances, for
		// a single cell.

		val xIndexBlock =  extrudedCellBlock.myCIRs(0)
		val chosenXPos : Range = xIndexBlock.getFirstCellIndex.getIndexFrom0 to xIndexBlock.getLastCellIndex.getIndexFrom0
		val chosenYPos :  Range = 0 to yCount - 1
		val chosenZPos : Range = 2 to 5
		for (zIdx <- chosenZPos)
			for (yIdx <- chosenYPos)
				for (xIdx <- chosenXPos) {
					makeBoxWig(deepSpace, vizNode, outerGuy.myBrushJar.cyan_med,  quadMeshFiveByFive, seq, xIdx, yIdx, zIdx)
					seq += 1
				}


		info0("End Srtw.makeSheetspace")
	}

	def makeBoxWig(deepSpace: MultiDimGridSpace, vizNode : Node, mainBrush : Brush,  qMesh : Mesh, seq : Int, xi : Int, yi: Int, zi: Int) : Int = {
		// Make integer index space range, where 6 args are 3 pairs for x,y,z: (firstX, lastX), (firstY...
		val unitCB: CellBlock = CellRangeFactory.makeUnitBlock3D(xi, yi, zi)
		val unitPB: PosBlock = deepSpace.computePosBlockForCellBlock(unitCB)
		debug3("Unit cell with seq#={} has cellBlock={} and posBlock.description={}", seq : Integer, unitCB, unitPB.describe)
		val xpr: PosRange = unitPB.myPRs(0)
		val ypr: PosRange = unitPB.myPRs(1)
		val zpr: PosRange = unitPB.myPRs(2)
		val qlabTxt01: String = "bq_" + seq + "_1"
		val qg1: Geometry = new Geometry(qlabTxt01, qMesh)
		mainBrush.stroke(qg1)  // TrialNexus uses bsc1 here
		qg1.setLocalTranslation(xpr.getMin, ypr.getMin, zpr.getMin)
		vizNode.attachChild(qg1)
		val qlabBT_01: BitmapText = myFirstTSF.makeTextSpatial(qlabTxt01, 0.2f, RenderQueue.Bucket.Transparent, 6)
		qlabBT_01.setLocalTranslation(xpr.getCenter, ypr.getCenter, zpr.getCenter)
		vizNode.attachChild(qlabBT_01)

		val dmaker = new DoodleMaker(mainBrush.myBSC, qMesh) // TrialNexus uses bsc2 here

		val doodle : Geometry = dmaker.makeDoodle(seq, xpr.getMax, ypr.getCenter, zpr.getCenter)
		val whatIdx : Int = vizNode.attachChild(doodle)
		-1
	}
	def translateToMins (g : Geometry) = ???

	def makePlaceAndAttach() = ???

}
class DoodleMaker(bsc : BlendedShapeContext, qMesh : Mesh) {
	lazy val myRot = new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_Y)
	def getRot : Quaternion = myRot
	def makeDoodle(seq: Int, xf : Float, yf : Float, zf : Float) : Geometry = {
		val qlabTxt02: String = "bq_" + seq + "_2"
		val qg2: Geometry = new Geometry(qlabTxt02, qMesh)
		bsc.setupGeom(qg2)
		val spat : Spatial = qg2
		// qg2.setLocalTranslation(xpr.getMin, ypr.getMin, zpr.getMin)
		spat.setLocalTranslation(xf, yf, zf)
		val rotAboutY_90: Quaternion = getRot
		spat.setLocalRotation(rotAboutY_90)
		qg2
	}
}
class MatPallete(myBaseMat: Material) {
	val myBaseBSC: BlendedShapeContext = new BlendedShapeContext(myBaseMat)
	myBaseBSC.setRenderStateVals(myBaseMat)

	def getBSC_forColor(crgba : ColorRGBA) : BlendedShapeContext = {
		val localMat: Material = myBaseMat.clone
		localMat.setColor("Color", crgba)
		new BlendedShapeContext(myBaseBSC, localMat)
	}
}
class Brush(myMatPal : MatPallete, myColor : ColorRGBA) {
	lazy val myBSC = myMatPal.getBSC_forColor(myColor)

	def stroke(geom : Geometry) : Unit = {
		myBSC.setupGeom(geom)
	}
}
trait SpatPlacer {
	def place(spat : Spatial) : Unit
}
class DoodlePlacer(xf : Float, yf : Float, zf : Float, rotQuat : Quaternion) extends SpatPlacer {
	override def place(spat : Spatial) : Unit = {
		spat.setLocalTranslation(xf, yf, zf)
		spat.setLocalRotation(rotQuat)
	}
}
trait SpatMaker  {
	def makeSpat(nam : String) : Spatial
}
class TextSpatMaker(myTSF: TextSpatialFactory) extends SpatMaker {
	def makeBitmapTxt(txt : String): BitmapText = {
		myTSF.makeTextSpatial(txt, 0.2f, RenderQueue.Bucket.Transparent, 6)
	}
	override def makeSpat(nam : String) : Spatial = makeBitmapTxt(nam)
}
class MeshGeoMaker(myMesh : Mesh, myBrush : Brush) {
	def makeGeom(nam : String) : Geometry = {
		val geo: Geometry = new Geometry(nam, myMesh)
		myBrush.stroke(geo)
		geo
	}
}

class BrushJar(myMatPal : MatPallete) {
	def applyAlpha(baseColor : ColorRGBA, alpha : Float ) : ColorRGBA = {
		new ColorRGBA(baseColor.getRed, baseColor.getGreen, baseColor.getBlue, alpha)
	}
	val zeroAlpha : Float = 0.0f
	val thinAlpha : Float = 0.25f
	val mediumAlpha : Float = 0.5f
	val thickAlpha : Float = 0.75f
	val fullAlpha : Float = 1.0f

	def makeBrush(color : ColorRGBA) : Brush = new Brush(myMatPal, color)

	def makeBrushDil(baseColor : ColorRGBA, alpha : Float) : Brush = {
		val dilutedColor = applyAlpha(baseColor, alpha)
		makeBrush(dilutedColor)
	}
	def makeBrushThin(baseColor : ColorRGBA) = makeBrushDil(baseColor, thinAlpha)
	def makeBrushMed(baseColor : ColorRGBA) = makeBrushDil(baseColor, mediumAlpha)
	def makeBrushThick(baseColor : ColorRGBA) = makeBrushDil(baseColor, thickAlpha)

	val black_thin = makeBrushThin(ColorRGBA.Black)

	val cyan_med = makeBrushMed(ColorRGBA.Cyan)

	val blue_med = makeBrushMed(ColorRGBA.Blue)

	val brown_med = makeBrushMed(ColorRGBA.Brown)

	val orange_med = makeBrushMed(ColorRGBA.Orange)

	val purply = makeBrush(new ColorRGBA(0.5f, 0.1f, 0.9f, 0.5f))
	val yellowy = makeBrush(new ColorRGBA(0.9f, 0.8f, 0.1f, 0.5f))
	val reddy = makeBrush(new ColorRGBA(1.0f, 0.0f, 0.0f, 0.7f))

}
class BscJar(myMatPal : MatPallete) {
	lazy val bsc_cyan: BlendedShapeContext = myMatPal.getBSC_forColor(ColorRGBA.Cyan)
	lazy val bsc_purply = myMatPal.getBSC_forColor(new ColorRGBA(0.5f, 0.1f, 0.9f, 0.5f))
	lazy val bsc_yellwy = myMatPal.getBSC_forColor(new ColorRGBA(0.9f, 0.8f, 0.1f, 0.5f))
	lazy val bsc_reddy = myMatPal.getBSC_forColor(new ColorRGBA(1.0f, 0.0f, 0.0f, 0.7f))

}
class GeomSrc()
