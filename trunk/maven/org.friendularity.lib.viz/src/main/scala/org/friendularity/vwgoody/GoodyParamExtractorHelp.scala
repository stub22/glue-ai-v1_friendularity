package org.friendularity.vwgoody

import com.jme3.math.Vector3f
import org.cogchar.render.app.entity.GoodyActionExtractor

/**
  * Created by Owner on 1/2/2017.
  */
trait GoodyParamExtractorHelp {
	def getScaleVectorFrom(goodyActionExtractor: GoodyActionExtractor): Vector3f = {
		var scaleVec: Vector3f = goodyActionExtractor.getScaleVec3f
		val scaleUniform: java.lang.Float = goodyActionExtractor.getScaleUniform
		if ((scaleVec == null) && (scaleUniform != null)) {
			scaleVec = new Vector3f(scaleUniform, scaleUniform, scaleUniform)
		}
		scaleVec
	}

	def getScaleUniformFrom(goodyActionExtractor: GoodyActionExtractor): java.lang.Float = {
		var scaleUniform: java.lang.Float = goodyActionExtractor.getScaleUniform
		val scaleVec: Vector3f = goodyActionExtractor.getScaleVec3f

		if ((scaleUniform == null) && (scaleVec != null)) {
			if (java.lang.Float.compare(scaleVec.getX, scaleVec.getY) == 0 && java.lang.Float.compare(scaleVec.getX, scaleVec.getZ) == 0) {
				scaleUniform = scaleVec.getX
			} else {
				throw new IllegalStateException("Could not find uniform scale in GoodyActionExtractor.")
			}
		}
		scaleUniform
	}

}
