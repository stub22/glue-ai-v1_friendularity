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
import com.jme3.math.{ColorRGBA, Quaternion}
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.{Geometry, Mesh, Spatial}
import org.cogchar.render.trial.TextSpatialFactory
import org.cogchar.render.trial.TrialNexus.BlendedShapeContext

/**
  * Created by Stub22 on 6/21/2016.
  */

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
	// BSC handles application of material, queueBucket, and cullHint.
	lazy val myBSC : BlendedShapeContext = myMatPal.getBSC_forColor(myColor)

	def stroke(geom : Geometry) : Unit = {
		myBSC.setupGeom(geom)
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

