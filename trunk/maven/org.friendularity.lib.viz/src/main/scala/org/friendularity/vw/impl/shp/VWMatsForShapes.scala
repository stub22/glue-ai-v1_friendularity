package org.friendularity.vw.impl.shp

import com.jme3.material.Material
import com.jme3.math.ColorRGBA
import com.jme3.scene.Geometry
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.vw.msg.shp.deep.VWMatDesc

/**
  * Created by Owner on 1/31/2017.
  */
trait SpatMatHelper {
	protected def getTooMuchRRC : RenderRegistryClient
	val rrc = getTooMuchRRC
	val myAssetMgr = rrc.getJme3AssetManager(null);
	val myUnshMat = new Material(myAssetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
	val myMatPal = new MatPallete(myUnshMat)
	val outerGuy = new OuterTestQuadsAndTextMaker(rrc, myMatPal)
	def getBrushJar : BrushJar = outerGuy.myBrushJar

	def cloneUnshaded : Material = myUnshMat.clone
}

trait MatsForShapes extends SpatMatHelper  {
	def getBestMat(matDesc: VWMatDesc) : Material = {
		val specialMat_opt = matDesc.makeSpecialMaterial_opt(rrc)

		if (specialMat_opt.isDefined) {
			specialMat_opt.get
		} else {
			val clr = matDesc.getColorParam_opt.getOrElse(ColorRGBA.Gray)
			val unsh = cloneUnshaded
			unsh.setColor("Color", clr)
			unsh
		}
	}
	def applyMat(geom : Geometry, matDesc: VWMatDesc) : Unit = {
		val rrc = getTooMuchRRC
		val specialMat_opt = matDesc.makeSpecialMaterial_opt(rrc)
		if (specialMat_opt.isDefined) {
			geom.setMaterial(specialMat_opt.get)
		} else {
			val dsc_opt : Option[ColorRGBA] = matDesc.getColorParam_opt
			val dsc = dsc_opt.getOrElse(ColorRGBA.Gray)
			val brush = getBrushJar.makeBrush(dsc)
			brush.stroke(geom)
		}
	}

}
