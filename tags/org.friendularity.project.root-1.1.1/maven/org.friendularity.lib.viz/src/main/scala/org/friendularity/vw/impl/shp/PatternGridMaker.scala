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
import com.jme3.scene.shape.Quad
import com.jme3.scene.{Node => JmeNode, Mesh}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.respire.Srtw

/**
  * Created by Owner on 1/19/2017.
  */
trait PatternGridMaker extends VarargsLogging {
	// We are currently calling this *off* rndThrd, and it is working.
	def makeBigGridAndAttach_onRendThrd(rrc: RenderRegistryClient, parentNode : JmeNode) : Unit = {
		val ssNode = makeBigGridNode(rrc)
		info2("Made cellGrid node, attaching: {} to {} - which is supposed to be on rendThrd, but conditionally may " +
					"work otherwise, too.", ssNode, parentNode)
		parentNode.attachChild(ssNode) // Question:  Is this the only step that requires us to be onRendThrd?
	}
	def makeBigGridNode(rrc: RenderRegistryClient) : JmeNode = {
		val assetMgr = rrc.getJme3AssetManager(null);
		val someMat = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
		val matPal = new MatPallete(someMat)
		val outerGuy = new OuterTestQuadsAndTextMaker(rrc, matPal)
		val srtwInst = new Srtw {
			override def getRRC = rrc
			override def getOuterGuy : OuterTestQuadsAndTextMaker = outerGuy

		}
		val uniformCount : Int = 40
		val uniformLen : Float = 800.0f

		// Decide the cross-sections of cells we want to do something with.
		// It is actually OK if these indices are outside the implied range of the deepSpace

		val chosenIdxs_X: Range = 10 to 30 by 2
		val chosenIdxs_Y: Range = 10 to 42 by 4
		val chosenIdxs_Z: Range = -5 to 30 by 5

		val ssNode = srtwInst.makeSheetspace(uniformCount, uniformLen, chosenIdxs_X, chosenIdxs_Y, chosenIdxs_Z)
		ssNode

	}
}
class OuterTestQuadsAndTextMaker(myRRC : RenderRegistryClient, myMatPal : MatPallete) {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(myRRC)
	val myBrushJar = new BrushJar(myMatPal)

	val quadMeshFiveByFive: Mesh = new Quad(5,5)

	val redQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)

}