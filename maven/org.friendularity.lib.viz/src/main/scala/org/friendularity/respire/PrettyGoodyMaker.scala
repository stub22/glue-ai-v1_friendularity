package org.friendularity.respire

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.renderer.Camera
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.{CameraBinding, VWorldEntity, GoodyActionExtractor}
import org.cogchar.render.goody.basic.{GoodyBox, VirtualFloor}
import org.cogchar.render.goody.bit.{TicTacGrid, TicTacMark, BitCube, BitBox}
import org.cogchar.render.goody.flat.{ParagraphGoody, ScoreBoardGoody, CrossHairGoody}
import org.cogchar.render.optic.goody.VWorldCameraEntity
import org.cogchar.render.sys.goody.GoodyRenderRegistryClient
import java.lang.{Float => JFloat, Integer => JInt }
/**
  * Created by Stub22 on 4/29/2016.
  *
  * This disabled code copy is here as a reminder of what Goodies previously "sort of worked" in the Cogchar ~2013 system.
  */
trait IgnorePrematureCopy
/*
class PrettyGoodyMaker 	(goodySpace : GoodySpace, myRRC: GoodyRenderRegistryClient) extends VarargsLogging {
		// Copied from org.cogchar.render.app.entity.GoodyFactory
		def createByAction(ga: GoodyActionExtractor): VWorldEntity = {
			var novGoody: VWorldEntity = null
			if (ga.getKind eq GoodyActionExtractor.Kind.CREATE) {
				try {
					var scaleVec: Vector3f = ga.getScaleVec3f
					val scaleUniform: JFloat = ga.getScaleUniform
					if ((scaleVec == null) && (scaleUniform != null)) {
						scaleVec = new Vector3f(scaleUniform, scaleUniform, scaleUniform)
					}
					val locVec: Vector3f = ga.getLocationVec3f
					val rotQuat: Quaternion = ga.getRotationQuaternion
					val goodyID: Ident = ga.getGoodyID
					val goodyType: Ident = ga.getType
					val gcolor: ColorRGBA = ga.getColor
					val goodyText: String = ga.getText
					val bitBoxState: Boolean = ga.getSpecialBoolean(GoodyNames.BOOLEAN_STATE)
					val isAnO: Boolean = ga.getSpecialBoolean(GoodyNames.USE_O)
					val rowCount: JInt = ga.getSpecialInteger(GoodyNames.ROWS)
					if (GoodyNames.TYPE_BIT_BOX == goodyType) {
						novGoody = new BitBox(myRRC, goodyID, locVec, rotQuat, scaleVec, bitBoxState)
					}
					else if (GoodyNames.TYPE_BIT_CUBE == goodyType) {
						novGoody = new BitCube(myRRC, goodyID, locVec, rotQuat, scaleVec, bitBoxState)
					}
					else if (GoodyNames.TYPE_FLOOR == goodyType) {
						novGoody = new VirtualFloor(myRRC, ga.getGoodyID, locVec, gcolor, true)
					}
					else if (GoodyNames.TYPE_TICTAC_MARK == goodyType) {
						novGoody = new TicTacMark(myRRC, goodyID, locVec, rotQuat, scaleVec, isAnO)
					}
					else if (GoodyNames.TYPE_TICTAC_GRID == goodyType) {
						novGoody = new TicTacGrid(myRRC, goodyID, locVec, rotQuat, gcolor, scaleVec)
					}
					else if (GoodyNames.TYPE_CROSSHAIR == goodyType) {
						novGoody = new CrossHairGoody(myRRC, goodyID, locVec, scaleUniform)
					}
					else if (GoodyNames.TYPE_SCOREBOARD == goodyType) {
						val sizeX: JFloat = ga.getSizeVec3D()(0)
						val rowHeight: JFloat = sizeX
						val textSize: JFloat = scaleUniform
						info4("Scoreboard row count={}, rowHeight={}, textSize={}, locVec={}", rowCount : JInt, rowHeight : JFloat, textSize : JFloat, locVec)
						novGoody = new ScoreBoardGoody(myRRC, goodyID, locVec, rowHeight, rowCount, textSize)
					}
					else if (GoodyNames.TYPE_TEXT == goodyType) {
						novGoody = new ParagraphGoody(myRRC, goodyID, locVec, scaleVec.getX, gcolor, goodyText)
					}
					else if (GoodyNames.TYPE_BOX == goodyType) {
						novGoody = new GoodyBox(myRRC, goodyID, locVec, rotQuat, gcolor, scaleVec)
					}
					else if (GoodyNames.TYPE_CAMERA == goodyType) {
						val cameraUri: Ident = goodyID
						if (goodySpace.getGoody(cameraUri) == null) {
							info1("Adding a VWorldCameraEntity for {}", cameraUri)
							val camBinding: CameraBinding = myRRC.getOpticCameraFacade(null).getCameraBinding(cameraUri)
							if (camBinding != null) {
								val cam: Camera = camBinding.getCamera
								if (cam != null) {
									novGoody = (new VWorldCameraEntity(myRRC, cameraUri, cam))
								}
								else {
									throw new RuntimeException("No actual camera found in binding at " + cameraUri)
								}
							}
							else {
								warn1("Couldn't find camera with URI {} for goody", cameraUri)
							}
						}
					}
					else {
						warn1("Did not recognize requested goody type for creation: {}", ga.getType)
					}
				}
				catch {
					case e: Exception => {
						error2("Error attempting to create goody {}, exc={}", ga.getGoodyID, e : Object)
					}
				}
			}
			else {
				warn1("GoodyFactory received request to add a goody, but the GoodyAction kind was not CREATE! Goody URI: {}", ga.getGoodyID)
			}
			return novGoody
		}
	}
*/
/*
	 VWorldEntity createAndAttachByAction(ga : GoodyActionExtractor, qStyle : Queuer.QueueingStyle) {
  val newGoody : VWorldEntity = createByAction(ga)
if (newGoody != null) {
newGoody.attachToVirtualWorldNode(myRootNode, qStyle)
}
return newGoody
}


	@Override public ConsumpStatus consumeAction(ThingActionSpec actionSpec, Ident srcGraphID_isUnused) {
		theLogger.info("The targetThingType is {}", actionSpec.getTargetThingTypeID()); // TEST ONLY

		// How do we decide whether it's really a VWorld / Goody action?
		// Below, the targetThing is presumed to be a "goody", either existing or new.
		GoodyActionExtractor ga = new GoodyActionExtractor(actionSpec);
		Ident gid = ga.getGoodyID();
		VWorldEntity goodyOne = myGoodiesByID.get(gid);

		GoodyActionExtractor.Kind kind = ga.getKind();
		theLogger.info("The kind of Goody inspected is {}", kind); // TEST ONLY
		if (kind != null) {
			switch (ga.getKind()) {
				case CREATE: { // If it's a CREATE action, we will do some different stuff
					if (myGoodiesByID.containsKey(gid)) {
						theLogger.warn("Goody already created! Ignoring additional creation request for goody: {}", gid);
					} else {
						goodyOne = GoodyFactory.getTheFactory().createAndAttachByAction(ga, VWorldEntity.QueueingStyle.QUEUE_AND_RETURN);
						if (goodyOne != null) {
							addGoody(goodyOne);
							return ConsumpStatus.USED;
						}
					}
					break;
				}
				case DELETE: {
					if (!myGoodiesByID.containsKey(gid)) {
						theLogger.warn("Could not delete goody because it does not exist: {}", gid);
					} else {
						removeGoody(goodyOne);
						return ConsumpStatus.USED;
					}
					break;
				}
				default: {
					// For the moment, let's focus on "update"
					try {
						// Now - apply the action to goodyOne
						goodyOne.applyAction(ga, VWorldEntity.QueueingStyle.QUEUE_AND_RETURN);
						return ConsumpStatus.USED;
					} catch (Exception e) {
						theLogger.warn("Problem attempting to update goody with URI: {}", gid, e);
					}
				}
			}
		}
		return ConsumpStatus.IGNORED;
	}

	  def removeGoody(departingGoody : VWorldEntity){
departingGoody.detachFromVirtualWorldNode(VWorldEntity.QueueingStyle.QUEUE_AND_RETURN)
myGoodiesByID.remove(departingGoody.getUri)
}

	  def addGoody(newGoody : VWorldEntity){
if (newGoody != null) {
  val goodyUri : Ident = newGoody.getUri
theLogger.info("Adding Goody with URI: {}", goodyUri)
myGoodiesByID.put(goodyUri, newGoody)
}
else {
theLogger.warn("Something is attempting to add a null goody to the GoodySpace, ignoring")
}
}

	  def attachGoodyNode{
  val dsm : DeepSceneMgr = myRRC.getSceneDeepFacade(null)
myRRC.getWorkaroundAppStub.enqueue(new Callable[Void]() {
@throws(classOf[Exception])
  def call : Void = {
dsm.attachTopSpatial(myRootNode)
return null
}
})
}

 */