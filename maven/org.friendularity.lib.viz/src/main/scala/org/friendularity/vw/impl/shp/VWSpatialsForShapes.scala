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

import com.jme3.material.Material
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.shape.{Box, PQTorus, Torus, Cylinder, Sphere}

import com.jme3.scene.{Geometry, Mesh, Node => JmeNode, Spatial}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.vw.mprt.manip.Transform3D
import org.friendularity.vw.msg.shp.deep.{VWMD_Box, VWMD_PQTorus, VWMatDesc, VWMeshDesc, VWMD_Torus, VWMD_Cylinder, VWMD_Sphere, CompositeMeshyShapeCreateRq, VWSCR_CellGrid, VWSCR_TextBox, VWSCR_ExistingNode, VWSCR_CamGuideNode, VWSCR_Node, VWShapeCreateRq}

/**
  * Code moved to new file on 1/19/2017.
  */
trait SpatMatHelper {
	protected def getTooMuchRRC : RenderRegistryClient
	val rrc = getTooMuchRRC
	val myAssetMgr = rrc.getJme3AssetManager(null);
	val myUnshMat = new Material(myAssetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
	val myMatPal = new MatPallete(myUnshMat)
	val outerGuy = new OuterTestQuadsAndTextMaker(rrc, myMatPal)
	def getBrushJar : BrushJar = outerGuy.myBrushJar
}


// Interprets any ShapeCreateRq to produce a new JmeSpatial, or extract one from the request itself

trait VWSpatialsForShapes extends PatternGridMaker with SpatMatHelper {


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
			case txtBox : VWSCR_TextBox => {
				null
			}
			case bigGrid : VWSCR_CellGrid => {
				makeBigGridNode(getTooMuchRRC)
			}
			case cmpndMeshyRq : CompositeMeshyShapeCreateRq => {
				val meshDescPart = cmpndMeshyRq.getMeshDescPart
				val geom : Geometry = makeMeshFromDesc(meshDescPart)
				val matDescPart = cmpndMeshyRq.getMatDescPart
				applyMat(geom, matDescPart)

				geom
			}
				/*
			case meshBasedRq : VWMeshyDesc => {
				makeMeshFromDesc(meshBasedRq)
			}
			*/
		}
	}
	def makeMeshFromDesc(meshBasedRq : VWMeshDesc) : Geometry = {
		val mesh: Mesh = meshBasedRq match {
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
		}
		val geomNameArb: String = "geom_from_msg_shape_" + System.currentTimeMillis()
		val geom = new Geometry(geomNameArb, mesh)
//		applySpatialTransform_full(geom, meshBasedRq.getCoreParams3D_opt.get)
		geom

	}
	def applyMat(geom : Geometry, matDesc: VWMatDesc) : Unit = {
		val dsc_opt : Option[ColorRGBA] = matDesc.getColorParam_opt
		val dsc = dsc_opt.getOrElse(ColorRGBA.Gray)
		val brush = getBrushJar.makeBrush(dsc)
		brush.stroke(geom)
	}
	def applySpatialTransform_full(spat : Spatial, params : Transform3D) : Unit = {
		val pos : Vector3f = params.getPos
		spat.setLocalTranslation(pos)
		val rot : Quaternion = params.getRotQuat
		spat.setLocalRotation(rot)
		val scl : Vector3f = params.getScale
		spat.setLocalScale(scl)
	}
}
