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

package org.friendularity.vw.impl.shp

import com.jme3.font.BitmapText
import com.jme3.material.Material
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.shape.{Box, PQTorus, Torus, Cylinder, Sphere}

import com.jme3.scene.{Geometry, Mesh, Node => JmeNode, Spatial}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.tmpgood.tgbit.TG_BitCubeBox
import org.friendularity.vw.mprt.manip.Transform3D
import org.friendularity.vw.msg.shp.deep.{VWMD_TexturedBox, VWMD_Box, VWMD_PQTorus, VWMatDesc, VWMeshDesc, VWMD_Torus, VWMD_Cylinder, VWMD_Sphere, CompositeMeshyShapeCreateRq, VWSCR_CellGrid, VWSCR_TextBox, VWSCR_ExistingNode, VWSCR_CamGuideNode, VWSCR_Node, VWShapeCreateRq}

/**
  * Code moved to new file on 1/19/2017.
  */

class TextSpatMkrWrpr(myRRC : RenderRegistryClient) {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(myRRC)
	val happyTxtMaker = new TextSpatMaker(myFirstTSF) {
		override val renderScale_meansWhat : Float = 0.8f
		override val rectWidth_relatesToCharsPerScaleUnitOrWhat = 96
	}

	def makeTextSpat(txtBoxRq : VWSCR_TextBox) : BitmapText = {
		val inFltSpc = txtBoxRq.inFlatSpace
		val txtCntnt = txtBoxRq.contentTxt
		val clr = txtBoxRq.color
		val initXform = txtBoxRq.getInitXform3D_partial

		val btmpTxt : BitmapText = if (inFltSpc)
			happyTxtMaker.makeBitmapTxt2D(txtCntnt)
		else
			happyTxtMaker.makeBitmapTxt3D(txtCntnt)

		// Since BitmapText supports this method directly, we don't have to make a material and apply it...?
		btmpTxt.setColor(clr)

		btmpTxt
	}
	// myOverlayText.setSize(myOverlayText.getFont.getCharSet.getRenderedSize * scale)
	// myOverlayText.setColor(color)
	// myOverlayText = myRenderRegCli.getSceneTextFacade(null).getScaledBitmapText(myContent, myScale)

}

// Interprets any ShapeCreateRq to produce a new JmeSpatial, or extract one from the request itself

trait VWSpatialsForShapes extends PatternGridMaker with MatsForShapes  {

	lazy val txtSpatMW = new TextSpatMkrWrpr(getTooMuchRRC)

	//	val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
	def makeOrExtractSpat(vwscr : VWShapeCreateRq) : Spatial = {
		vwscr match {
			case aNode : VWSCR_Node => {
				val madeNode: JmeNode = new JmeNode("generic_node_" + System.currentTimeMillis())
				madeNode
			}
			case cgNode : VWSCR_CamGuideNode => {
				val madeNode: JmeNode = new JmeNode("camGuide_node_" + System.currentTimeMillis())
				madeNode
			}
			case existingNode :	VWSCR_ExistingNode => {
				existingNode.existingNode
			}
			// We treat Txt specially because it might be either 2D or 3D, which would place it in a different
			// renderBucket.
			case txtBox : VWSCR_TextBox => {
				txtSpatMW.makeTextSpat(txtBox)
			}
			case bigGrid : VWSCR_CellGrid => {
				makeBigGridNode(getTooMuchRRC)
			}
			case cmpndMeshyRq : CompositeMeshyShapeCreateRq => {
				val meshDescPart = cmpndMeshyRq.getMeshDescPart
				val geom : Geometry = makeMeshAndGeomFromDesc(meshDescPart)
				val matDescPart = cmpndMeshyRq.getMatDescPart
				applyMatFromBrushOrSpecial(geom, matDescPart)
				geom
			}
		}
	}
	def makeMeshAndGeomFromDesc(meshDesc : VWMeshDesc) : Geometry = {
		val mesh: Mesh = meshDesc match {
			case sph: VWMD_Sphere => {
				// zSamp, rSamp, radius
				new Sphere(sph.zSamples, sph.radialSamples, sph.radiusF)
			}
			case cyl: VWMD_Cylinder => {
				// REVIEW:  5 arg version of constructor used here.
				// Other 2 args are:  float radius2, boolean inverted
				new Cylinder(cyl.axisSamples, cyl.radialSamples, cyl.radius, cyl.height, cyl.closed)
			}
			case tor : VWMD_Torus => {
				new Torus(tor.circleSamples, tor.radialSamples, tor.innerRadius, tor.outerRadius)// 40, 20, 1f / 5f, 5f / 6f)
			}
			case pqTor : VWMD_PQTorus => {
				new PQTorus(pqTor.p, pqTor.q, pqTor.radius, pqTor.width, pqTor.steps, pqTor.radialSamples)
			}
			case bx : VWMD_Box => {
				new Box(bx.xSize, bx.ySize, bx.zSize)
			}
			case tbx : VWMD_TexturedBox => {
				val msh = new TG_BitCubeBox(tbx.xSize, tbx.ySize, tbx.zSize)
				msh
			}
		}
		val geomNameArb: String = "geom_from_msg_shape_" + System.currentTimeMillis()
		val geom = new Geometry(geomNameArb, mesh)
		geom
	}
	def UNUSED_applySpatialTransform_full_UNUSED_I_THINK(spat : Spatial, params : Transform3D) : Unit = {
		val pos : Vector3f = params.getPos
		spat.setLocalTranslation(pos)
		val rot : Quaternion = params.getRotQuat
		spat.setLocalRotation(rot)
		val scl : Vector3f = params.getScale
		spat.setLocalScale(scl)
	}
}
