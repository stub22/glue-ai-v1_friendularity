package org.friendularity.vw.msg.shp.deep

import org.friendularity.vw.mprt.manip.MaybeTransform3D

/**
  * Created by Owner on 1/23/2017.
  */
trait CompositeMeshyShapeCreateRq extends VWShapeCreateRq with ExposedMatParamsPart {
	def getKnownIdentsPart : KnowsShapeIDsPart
	def getMeshDescPart : VWMeshDesc
	// def getMatDescPart : VWMatDesc

	override def getKnownID_opt  = getKnownIdentsPart.getKnownID_opt
	override def getKnownParentID_opt  = getKnownIdentsPart.getKnownParentID_opt

//	override def getColorParam_opt = getMatDescPart.getColorParam_opt

	//	override def getCoreParams3D_opt = getMeshyDescPart.getCoreParams3D_opt
	// override def getPosParam_opt  = getMeshyDescPart.getPosParam_opt
	// override def getRotParam_opt  = getMeshyDescPart.getRotParam_opt

}


// Arguable whether this compound msg should be a case class, with a VWSCR_ name.  Hmmm.
case class VWSCR_MeshyComposite(idsPart : KnowsShapeIDsPart, initXform3D : MaybeTransform3D,
								meshyPart : VWMeshDesc, matPart : VWMatDesc)
			extends  CompositeMeshyShapeCreateRq {

	override def getKnownIdentsPart: KnowsShapeIDsPart = idsPart

	override def getInitXform3D_partial : MaybeTransform3D = initXform3D

	override def getMeshDescPart: VWMeshDesc = meshyPart

	override def getMatDescPart : VWMatDesc = matPart
	// override def getInitXform3D_partial : MaybeTransform3D = initXform3D
}

