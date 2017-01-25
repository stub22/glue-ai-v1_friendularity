package org.friendularity.vw.cli.goshcl

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{TypedValueMap, ThingActionSpec}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.MaybeTransform3D
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{VWMeshDesc, VWMatDesc, KnowsShapeIDsPart, VWSCR_MeshyComposite}
import org.friendularity.vwmsg.{OrdinaryParams3D, CoreParams3D}

/**
  *
  * GoodyRqToShaperRqTranslator
  *
  * Created by Stub22 on 1/22/2017.
  *
  * This file instantiates goody-shaper  mutable state, by instantiating a MadeGoodyCache.
  * Around that state, this file defines top level action dispatch of the goody-xlator system.
  *
  * We have one trait for handling create requests, one for handling other=existing-target requests,
  * one to dispatch to the first two, based on TA-verbID
  */

trait HandleGoodyCreateRequest extends SubTransChooser with KnowsGoodyCache with VarargsLogging {

	def makeCreationRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		debug3("Processing create for tgtID={}, tgtTypeID={}.  No fancy switching for verbID={}",  tgtTypeID, tgtID, verbID)
		val ourCache : MadeGoodyCache = getMGCache
		if (ourCache.hasMGRecAtID(tgtID)) {
			throw new RuntimeException("Attempted re-creation of already-made goody at ID=" + tgtID)
		}
		val xlator = chooseXlatorByThingType(tgtTypeID)

		val gax: GoodyActionExtractor = new GoodyActionExtractor(actSpec)


// 		val paramTVM = actSpec.getParamTVM
//		val shapeRqList : List[VWContentRq] = xlator.makeCreateRqs(verbID, tgtTypeID, tgtID, gax) // paramTVM)
		val shapeRqList : List[VWContentRq] = xlator.makeCreateRqs(actSpec)
		val madeRec = new MadeGoodyRec(tgtID, xlator)
		ourCache.storeMGRecAtID(madeRec)
		madeRec.recordXlation(actSpec, shapeRqList)
		shapeRqList
	}
}
trait HandleGoodyModifyRequest extends KnowsGoodyCache with VarargsLogging {

	// 3 public API methods
	def makeDelRqsFromTA	(actSpec : ThingActionSpec) : List[VWContentRq] = {
		operateOnExistingGoody(actSpec, opDel)
	}
	def makeMoveRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		operateOnExistingGoody(actSpec, opMove)
	}
	def makeSetRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		operateOnExistingGoody(actSpec, opSet)
	}

	// Shared impl method
	protected def operateOnExistingGoody (actSpec : ThingActionSpec,
										  oper : Function2[MadeGoodyRec, ThingActionSpec, List[VWContentRq]]) : List[VWContentRq] = {

		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		val ourCache : MadeGoodyCache = getMGCache
		if (ourCache.hasMGRecAtID(tgtID)) {
			val mgrec : MadeGoodyRec = ourCache.getMGRecAtID_opt(tgtID).get
			val shapeRqList = oper(mgrec, actSpec)
			mgrec.recordXlation(actSpec, shapeRqList)
			shapeRqList
		} else {
			error3("Cannot process TA-request with verb={}, goody not found at ID={}, TA={}", verbID, tgtID, actSpec)
			Nil
		}
	}

	// val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
	// Thinking:  One level of discrim we may want here is between flat and deep

	protected def opDel(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		mgrec.xlator.makeDeleteRqs(taSpec.getVerbID, taSpec.getTargetThingID)
	}
	protected def opMove(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		mgrec.xlator.makeMoveRqs(taSpec.getVerbID, taSpec.getTargetThingID, taSpec.getParamTVM)
	}
	protected def opSet(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		mgrec.xlator.makeSetRqs(taSpec.getVerbID, taSpec.getTargetThingID, taSpec.getParamTVM)
	}


}
trait GoodyRqToShaperRqTranslator extends KnowsGoodyCache {
	val ourCacheIsStateYo = new MadeGoodyCache {}
	override protected def getMGCache : MadeGoodyCache = ourCacheIsStateYo

	val createHandler = new HandleGoodyCreateRequest{
		override protected def getMGCache: MadeGoodyCache = ourCacheIsStateYo
	}
	val modifyHandler = new HandleGoodyModifyRequest{
		override protected def getMGCache: MadeGoodyCache = ourCacheIsStateYo
	}


	def makeContentRqsFromTA(actSpec : ThingActionSpec, sttIsNeeded : Int) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		verbID match {
			case GoodyNames.ACTION_CREATE => {
				createHandler.makeCreationRqsFromTA(actSpec)
			}
			case GoodyNames.ACTION_DELETE => {
				modifyHandler.makeDelRqsFromTA(actSpec)
			}
			case GoodyNames.ACTION_MOVE => {
				modifyHandler.makeMoveRqsFromTA(actSpec)
			}
			case GoodyNames.ACTION_SET => {
				modifyHandler.makeSetRqsFromTA(actSpec)
			}
		}
	}

}