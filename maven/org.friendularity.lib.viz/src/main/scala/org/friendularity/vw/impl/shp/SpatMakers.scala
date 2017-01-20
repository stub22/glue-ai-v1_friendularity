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
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.{Geometry, Mesh, Spatial}
import org.cogchar.render.trial.TextSpatialFactory

/**
  * Code moved to new file on 1/19/2017.
  */
trait SpatMaker2D  {
	def makeSpat2D(nam : String) : Spatial
}
trait SpatMaker3D  {
	def makeSpat3D(nam : String) : Spatial
}
class TextSpatMaker(myTSF: TextSpatialFactory) extends SpatMaker3D with SpatMaker2D {
	val renderScale_meansWhat : Float = 0.2f
	val rectWidth_relatesToCharsPerScaleUnitOrWhat = 12

	def makeBitmapTxt3D(txt : String): BitmapText = {
		myTSF.makeTextSpatial(txt, renderScale_meansWhat, RenderQueue.Bucket.Transparent, rectWidth_relatesToCharsPerScaleUnitOrWhat)
	}
	def makeBitmapTxt2D(txt : String): BitmapText = {
		val bucket = RenderQueue.Bucket.Gui // or can use null => Inherit
		myTSF.makeTextSpatial(txt, renderScale_meansWhat, bucket, rectWidth_relatesToCharsPerScaleUnitOrWhat)
	}
	override def makeSpat2D(nam : String) : Spatial = makeBitmapTxt2D(nam)
	override def makeSpat3D(nam : String) : Spatial = makeBitmapTxt3D(nam)
}

class MeshGeoMaker(myMesh : Mesh, myBrush : Brush)  extends SpatMaker3D with SpatMaker2D  {
	def makeGeom(nam : String) : Geometry = {
		val geo: Geometry = new Geometry(nam, myMesh)
		myBrush.stroke(geo)
		geo
	}
	override def makeSpat2D(nam : String) : Spatial = makeGeom(nam)
	override def makeSpat3D(nam : String) : Spatial = makeGeom(nam)
}