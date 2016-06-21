package org.friendularity.vwimpl

import com.jme3.font.BitmapText
import com.jme3.material.Material
import com.jme3.math.{Vector3f, FastMath, Quaternion, ColorRGBA}
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.shape.Quad
import com.jme3.scene.{Geometry, Mesh, Node}
import org.cogchar.render.trial.TextSpatialFactory

/**
  * Created by Owner on 6/20/2016.
  */
trait FiniteGrid
	/* See Srtw
	def makeSheetspace(parentNode: Node, baseMat: Material) {
		val xCount: Int = 7
		val yCount: Int = 5
		val zCount: Int = 9
		val deepSpace: Nothing = GridSpaceFactory.makeSpace3D(xCount, -40.0f, 40.0f, yCount, -20.0f, 20.0f, zCount, -50.0f, 20.0f)
		getLogger.info("Space description={}", deepSpace.describe)
		val extrudedCellBlock: Nothing = CellRangeFactory.makeBlock3D(3, 5, -1, 6, 2, 7)
		val extrudedPosBlock: Nothing = deepSpace.computePosBlockForCellBlock(extrudedCellBlock)
		getLogger.info("Computed result PosBlock description={}", extrudedPosBlock.describe)
		val vizNode: Node = new Node("sspace_viz_node")
		parentNode.attachChild(vizNode)
		val localMat1: Material = baseMat.clone
		localMat1.setColor("Color", new ColorRGBA(0.5f, 0.1f, 0.9f, 0.5f))
		val localMat2: Material = baseMat.clone
		localMat2.setColor("Color", new ColorRGBA(0.9f, 0.8f, 0.1f, 0.5f))
		val bscBase: TrialNexus.BlendedShapeContext = new TrialNexus.BlendedShapeContext(baseMat)
		bscBase.setRenderStateVals(baseMat)
		val bsc1: TrialNexus.BlendedShapeContext = new TrialNexus.BlendedShapeContext(bscBase, localMat1)
		val bsc2: TrialNexus.BlendedShapeContext = new TrialNexus.BlendedShapeContext(bscBase, localMat2)
		val tsf: TextSpatialFactory = new TextSpatialFactory(myRRC)
		val cellCount: Int = xCount * yCount * zCount
		var seq: Int = 0
		val quadMeshFiveByFive: Mesh = new Quad(5, 5)
		{
			var xi: Int = 1
			while (xi <= xCount) {
				{
					{
						var yi: Int = 1
						while (yi <= yCount) {
							{
								{
									var zi: Int = 1
									while (zi <= zCount) {
										{
											seq += 1
											val unitCB: Nothing = CellRangeFactory.makeUnitBlock3D(xi, yi, zi)
											val unitPB: Nothing = deepSpace.computePosBlockForCellBlock(unitCB)
											getLogger.debug("Unit cell with seq#={} has cellBlock={} and posBlock.description={}", seq, unitCB, unitPB.describe)
											val xpr: Nothing = unitPB.myPRs(0)
											val ypr: Nothing = unitPB.myPRs(1)
											val zpr: Nothing = unitPB.myPRs(2)
											val qlabTxt01: String = "bq_" + seq + "_1"
											val qg1: Geometry = new Geometry(qlabTxt01, quadMeshFiveByFive)
											bsc1.setupGeom(qg1)
											qg1.setLocalTranslation(xpr.getMin, ypr.getMin, zpr.getMin)
											vizNode.attachChild(qg1)
											val qlabBT_01: BitmapText = tsf.makeTextSpatial(qlabTxt01, 0.2f, RenderQueue.Bucket.Transparent, 6)
											qlabBT_01.setLocalTranslation(xpr.getCenter, ypr.getCenter, zpr.getMin)
											vizNode.attachChild(qlabBT_01)
											val qlabTxt02: String = "bq_" + seq + "_2"
											val qg2: Geometry = new Geometry(qlabTxt02, quadMeshFiveByFive)
											bsc2.setupGeom(qg2)
											qg2.setLocalTranslation(xpr.getMin, ypr.getMin, zpr.getMin)
											val rotAboutY_90: Quaternion = new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_Y)
											qg2.setLocalRotation(rotAboutY_90)
											vizNode.attachChild(qg2)
										}
										({
											zi += 1; zi - 1
										})
									}
								}
							}
							({
								yi += 1; yi - 1
							})
						}
					}
				}
				({
					xi += 1; xi - 1
				})
			}
		}
	}

}
*/
