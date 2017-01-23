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
import com.jme3.scene.shape.{Torus, Cylinder, Sphere}

import com.jme3.scene.{Geometry, Mesh, Node => JmeNode, Spatial}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.vw.mprt.manip.Transform3D
import org.friendularity.vw.msg.shp.deep.{VWSCR_Torus, VWSCR_Cylinder, VWSCR_Sphere, VWMeshyShapeRq, TwoPartMeshyShapeRq, VWSCR_CellGrid, VWSCR_TextBox, VWSCR_ExistingNode, VWSCR_CamGuideNode, VWSCR_Node, VWShapeCreateRq}

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
			case twoPartMeshyRq : TwoPartMeshyShapeRq => {
				val meshDescPart = twoPartMeshyRq.getMeshyDescPart
				val geom = makeMeshFromDesc(meshDescPart)
				geom
			}
			case meshBasedRq : VWMeshyShapeRq => {
				makeMeshFromDesc(meshBasedRq)
			}
		}
	}
	def makeMeshFromDesc(meshBasedRq : VWMeshyShapeRq) : Spatial = {
		val mesh: Mesh = meshBasedRq match {
			case sph: VWSCR_Sphere => {
				// zSamp, rSamp, radius
				new Sphere(16, 16, sph.myRadius)
			}
			case cyl: VWSCR_Cylinder => {
				// REVIEW:  5 arg version of constructor used here.
				// Other 2 args are:  float radius2, boolean inverted
				new Cylinder(cyl.axisSamples, cyl.radialSamples, cyl.radius, cyl.height, cyl.closed)
			}
			case tor : VWSCR_Torus => {
				new Torus(40, 20, 1f / 5f, 5f / 6f)
			}
		}
		val geomNameArb: String = "geom_from_msg_shape_" + System.currentTimeMillis()
		val geom = new Geometry(geomNameArb, mesh)
		applyMat(geom, meshBasedRq)
		applySpatialTransform(geom, meshBasedRq.getCoreParams3D.get)
		geom

	}
	def applyMat(geom : Geometry, mshShpRq : VWMeshyShapeRq) : Unit = {
		val dsc_opt : Option[ColorRGBA] = mshShpRq.getColorParam
		val dsc = dsc_opt.getOrElse(ColorRGBA.Gray)
		val brush = getBrushJar.makeBrush(dsc)
		brush.stroke(geom)
	}
	def applySpatialTransform(spat : Spatial, params : Transform3D) : Unit = {
		val pos : Vector3f = params.getPos
		spat.setLocalTranslation(pos)
		val rot : Quaternion = params.getRotQuat
		spat.setLocalRotation(rot)
		val scl : Vector3f = params.getScale
		spat.setLocalScale(scl)
	}
}
