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
package org.friendularity.vwimpl

import com.jme3.font.BitmapText
import com.jme3.material.Material
import com.jme3.math.{Quaternion, ColorRGBA}
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.shape.Quad
import com.jme3.scene.{Mesh, Spatial, Geometry}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.cogchar.render.trial.TrialNexus.BlendedShapeContext

/**
  * Created by Stub22 on 6/21/2016.
  */
class VWBrush {

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
	def makeBitmapTxt3D(txt : String): BitmapText = {
		myTSF.makeTextSpatial(txt, 0.2f, RenderQueue.Bucket.Transparent, 6)
	}
	def makeBitmapTxt2D(txt : String): BitmapText = {
		myTSF.makeTextSpatial(txt, 0.2f, RenderQueue.Bucket.Gui, 6)
	}
	override def makeSpat(nam : String) : Spatial = makeBitmapTxt3D(nam)
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

