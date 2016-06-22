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
  * Created by Owner on 6/21/2016.
  */
class VWBrush {

}
class OuterGuy(myRRC : RenderRegistryClient, myMatPal : MatPallete) {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(myRRC)
	val myBrushJar = new BrushJar(myMatPal)

	val quadMeshFiveByFive: Mesh = new Quad(5,5)

	val redQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)


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

