package org.friendularity.dull

import com.hp.hpl.jena
import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator, Literal}

import java.util.{List => JList, ArrayList => JArrayList, Set => JSet, Random}
import java.lang.{Long => JLong}

import org.slf4j.Logger

import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader
import org.appdapter.core.item.{JenaResourceItem, Item}
import org.appdapter.core.item.Item.LinkDirection
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.query.{SolutionList, Solution, SolutionHelper}
import org.appdapter.fancy.rclient.RepoClient
import org.cogchar.api.fancy.FancyThingModelWriter
import org.cogchar.api.thing.{SerTypedValueMap, ThingActionSpec}
import org.cogchar.impl.thing.basic.{BasicTypedValueMapWithConversion, BasicTypedValueMap, BasicThingActionSpec}
import org.cogchar.name.dir.NamespaceDir
import org.cogchar.name.thing.ThingCN


import scala.collection.mutable.ListBuffer

/**
  * Created by Owner on 6/11/2016.
  */

trait ThingActExposer extends VarargsLogging {
	// This code adapted from org.cogchar.impl.thing.basic.BasicThingActionSpecBuilder
	// which uses assembler form.
	def readActionFromJenaModel(jm: JenaModel, actionID : Ident): ThingActionSpec = {
		val btas = new BasicThingActionSpec()
		val actionRes = jm.getResource(actionID.getAbsUriString)
		val actionItem = new JenaResourceItem(actionRes)
		// So far reads basics, but not yet params.
		populateFieldsAndLinks(btas, actionItem)
		btas
	}

	protected def safeFreeIdent(uri: String): FreeIdent = {
		val mappedURI = if (uri.indexOf('#') == -1) {
			warn1("Found bare local name instead of URI, prefixing {} with TA_NS ", uri)
			ThingCN.TA_NS + uri
		} else uri
		return new FreeIdent(mappedURI)
	}

	private val propID_tgtThing: Ident = safeFreeIdent(ThingCN.P_targetThing)
//	private val propID_tgtThingType: Ident = safeFreeIdent(P_targetThingType)

	private val propID_rdfType : Ident = safeFreeIdent(NamespaceDir.RDF_NS + "type")


	private val propID_verb: Ident = safeFreeIdent(ThingCN.P_verb)
	private val propID_srcAgent: Ident = safeFreeIdent(ThingCN.P_sourceAgent)
	private val propID_postedTStamp: Ident = safeFreeIdent(ThingCN.P_postedTSMsec)

	private val propID_linkFromParamToAction : Ident = safeFreeIdent(ThingCN.P_IdentAttachedToThingAction)
	private val propID_paramKeyID : Ident = safeFreeIdent(ThingCN.P_paramIdent)
	private val propID_paramVal : Ident = safeFreeIdent(ThingCN.P_paramValue)

	protected def populateFieldsAndLinks(tgtSpec: BasicThingActionSpec, srcItem: Item) {

		tgtSpec.setMyActionRecordID(srcItem.getIdent)

		val sourceAgentItem: Item = srcItem.getOptionalSingleLinkedItem(propID_srcAgent, LinkDirection.FORWARD)
		if (sourceAgentItem != null) {
			tgtSpec.setMySourceAgentID(sourceAgentItem.getIdent)
		}

		// The current time is supplied here as a *backup* (i.e. default) value to be returned if no prop-val find.
		val postedTimestamp: Long = srcItem.getValLong(propID_postedTStamp, System.currentTimeMillis)
		tgtSpec.setMyPostedTimestamp(postedTimestamp)

		val targetThingItem: Item = srcItem.getOptionalSingleLinkedItem(propID_tgtThing, LinkDirection.FORWARD)
		if (targetThingItem != null) {
			tgtSpec.setMyTargetThingID(targetThingItem.getIdent)
		}

		val targetTypeItem: Item = targetThingItem.getOptionalSingleLinkedItem(propID_rdfType, LinkDirection.FORWARD)
		if (targetTypeItem != null) {
			tgtSpec.setMyTargetThingTypeID(targetTypeItem.getIdent)
		}

		val actionVerbItem: Item = srcItem.getOptionalSingleLinkedItem(propID_verb, LinkDirection.FORWARD)
		if (actionVerbItem != null) {
			tgtSpec.setMyActionVerbID(actionVerbItem.getIdent)
		}

		val btvm = new BasicTypedValueMapWithConversion

		readWeakParamsIntoBTVMap(btvm, srcItem)

		tgtSpec.setMyParamTVMap(btvm)
	}
	import scala.collection.JavaConversions._
	// Assumes "weak" param convention, where each param gets its own instanceURI
	def readWeakParamsIntoBTVMap( btvm : BasicTypedValueMap, actionItem : Item) : Unit = {
		val paramItems: JSet[Item] = actionItem.getLinkedItemSet(propID_linkFromParamToAction, LinkDirection.REVERSE)
		info1("Found {} paramItems", paramItems.size() : Integer)
		debug1("ParamItems dump={}", paramItems)
		for (pit <- paramItems) {
			val pitItem : Item = pit
			val pKeyID : Ident  = pitItem.getSingleLinkedItem(propID_paramKeyID, LinkDirection.FORWARD).getIdent
			// Consider:  We could produce a fancier TVM that uses model knowledge during field access.
			val pValString : String = pitItem.getValString(propID_paramVal, "ERR_NO_VAL")
			debug2("Storing param for key={}, val-string={}", pKeyID, pValString)
			// TODO:  If pValString is a URI, we should instead use     btvm.putNameAtName()
			btvm.putValueAtName(pKeyID, pValString)
		}

	}
	def extractThingActsFromModel(jenaModel : JenaModel) : List[ThingActionSpec] = {
	//	val RDF_NS = NamespaceDir.RDF_NS //   "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
		val typeProp = jenaModel.getProperty(propID_rdfType.getAbsUriString) //   RDF_NS, "type")

		val TA_TYPE : String = ThingCN.T_ThingAction
		val taTypeRes = jenaModel.getResource(TA_TYPE)

		val taResIter : ResIterator = jenaModel.listResourcesWithProperty(typeProp, taTypeRes)
		val resBuffer = new ListBuffer[Resource]
		while (taResIter.hasNext) {
			resBuffer.append(taResIter.nextResource())
		}
		val foundRsrcs : List[Resource] = resBuffer.toList
		info2("Found {} ThingAct resources: {}", foundRsrcs.length : Integer, foundRsrcs)

		val specBuffer = new ListBuffer[ThingActionSpec]
		for (rsrc <- foundRsrcs) {
			val actionID : Ident = new JenaResourceItem(rsrc)
			val actSpec : ThingActionSpec = readActionFromJenaModel(jenaModel, actionID)
			info4("extractThingsFromModel read actSpec of actionID={}, verb={}, tgtType={} tgtID={}",
				actionID, actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)

			debug1("Full dump of thingActionSpec:\n {}", actSpec)
			specBuffer.append(actSpec)
		}
		specBuffer.toList
	}
}

trait ThingActTurtleEncoder extends VarargsLogging {
	lazy val myRandomizer: Random = new Random

	def encodeAsTurtleMsg(actSpec : ThingActionSpec) : String = {
		val ftmw = new FancyThingModelWriter
		val specModelWithPrefixes : JenaModel  = ftmw.writeTASpecAndPrefixesToNewModel(actSpec, myRandomizer)

		val turtleTriplesString : String = ftmw.serializeSpecModelToTurtleString(specModelWithPrefixes)
		info2("Serialized turtle message FROM model of size {} triples TO string of length {} chars", specModelWithPrefixes.size() : JLong, turtleTriplesString.length : Integer)
		debug1("Debug-Dumping encoded turtle message:\n {}", turtleTriplesString)
		turtleTriplesString
	}
}

trait UnusedSolutionRowReadersWoo {
	// Not currently using this form, which we pasted from:
	// org.cogchar.impl.thing.routeBasicThingActionQResAdapter
// It was called by
/*	@Deprecated private List<ThingActionSpec> takeThingActions_Raw(RepoClient rc, Ident srcGraphID) {
		SolutionList actionsSolList = rc.queryIndirectForAllSolutions(ThingCN.ACTION_QUERY_URI, srcGraphID);
		BasicThingActionQResAdapter taqra = new BasicThingActionQResAdapter();
		List<ThingActionSpec> actionSpecList = taqra.reapActionSpecList(actionsSolList, rc, srcGraphID, SOURCE_AGENT_ID);
*/
	def reapActionSpecList(actionsList: SolutionList, rc: RepoClient, srcGraphID: Ident, srcAgentID: Ident,
						   theLogger: Logger): JList[ThingActionSpec] = {
		val actionSpecList: JList[ThingActionSpec] = new JArrayList[ThingActionSpec]
		val sh: SolutionHelper = new SolutionHelper
		import scala.collection.JavaConversions._
		for (actionSoln0 <- actionsList.javaList) {
			val actionSoln: Solution = actionSoln0.asInstanceOf[Solution]
			val actionID: Ident = sh.pullIdent(actionSoln, ThingCN.V_actionID)
			val verbID: Ident = sh.pullIdent(actionSoln, ThingCN.V_verbID)
			val targetID: Ident = sh.pullIdent(actionSoln, ThingCN.V_targetThingID)
			val targetTypeID: Ident = sh.pullIdent(actionSoln, ThingCN.V_targetThingTypeID)
			val actionParams: SerTypedValueMap = buildActionParameterValueMap(rc, srcGraphID, sh, actionID, theLogger)
			val tstampLiteral: Option[Literal] = Option(actionSoln.getLiteralResultVar(ThingCN.V_postedTStampMsec))
			val actionPostedTStampMsec: Long = tstampLiteral.map(_.getLong).getOrElse(-1)
			val spec: ThingActionSpec = new BasicThingActionSpec(actionID, targetID, targetTypeID, verbID, srcAgentID,
				actionParams, actionPostedTStampMsec)
			theLogger.debug("Read ThingAction (from solution row): {}", spec)
			actionSpecList.add(spec)
		}
		return actionSpecList
	}

	def buildActionParameterValueMap(rc: RepoClient, srcGraphID: Ident, sh: SolutionHelper,
									 actionIdent: Ident, theLogger: Logger): SerTypedValueMap = {
		val paramMap: BasicTypedValueMap = new BasicTypedValueMapWithConversion
		val paramList: SolutionList = rc.queryIndirectForAllSolutions(ThingCN.PARAM_QUERY_URI, srcGraphID, ThingCN.V_attachedActionID, actionIdent)
		import scala.collection.JavaConversions._
		for (paramSoln0 <- paramList.javaList) {
			val paramSoln: Solution = paramSoln0.asInstanceOf[Solution]
			val paramIdent: Ident = sh.pullIdent(paramSoln, ThingCN.V_actParamID)
			var paramValue: String = null
			try {
				val paramValueIdent: Ident = sh.pullIdent(paramSoln, ThingCN.V_actParamVal)
				paramValue = paramValueIdent.getAbsUriString
			}
			catch {
				case ex: ClassCastException => {
					paramValue = sh.pullString(paramSoln, ThingCN.V_actParamVal)
				}
			}
			theLogger.debug("Adding new param for Thing action {}: ident: {}, value: {}", Array[AnyRef](actionIdent, paramIdent, paramValue))
			paramMap.putValueAtName(paramIdent, paramValue)
		}
		return paramMap
	}

}
/*
ccrt:find_thing_action_params_99
      rdf:type ccrt:SparqlQuery , ccrt:NeedsPrefixHelp ;
      ccrt:queryText """SELECT ?actParamID ?actParamVal
WHERE { GRAPH ?qGraph {
?thingActionParam a ccrt:ThingActionParam;
ta:targetAction ?attachedToAction;
 ta:paramIdent ?actParamID ;
ta:paramValue ?actParamVal.
}}""" .
 */
/*
		val paramItems: util.Set[Item] = item.getLinkedItemSet(theThingActionParamAttachedTAFieldIdent, LinkDirection.FORWARD)
		val paramDictionary: BasicTypedValueMap = new BasicTypedValueMapWithConversion
		import scala.collection.JavaConversions._
		for (i <- paramItems) {
			var name: Ident = null
			val nameItem_RawSet: util.Set[Item] = i.getLinkedItemSet(theParamIdentFieldIdent, Item.LinkDirection.FORWARD)
			if (nameItem_RawSet == null && nameItem_RawSet.size != 1) {
				logger.warn("ThingActionParam \"{}\" did not provide a \"paramIdent\" and was discarded", i.getIdent)
				continue //todo: continue is not supported
			}
			else {
				name = nameItem_RawSet.iterator.next.getIdent
			}
			val identTypeItems: util.Set[Item] = i.getLinkedItemSet(theParamIdentValueFieldIdent, Item.LinkDirection.FORWARD)
			val stringTypeItems: util.Set[Item] = i.getLinkedItemSet(theParamStringValueFieldIdent, Item.LinkDirection.FORWARD)
			val intTypeItems: util.Set[Item] = i.getLinkedItemSet(theParamIntValueFieldIdent, Item.LinkDirection.FORWARD)
			val floatTypeItems: util.Set[Item] = i.getLinkedItemSet(theParamFloatValueFieldIdent, Item.LinkDirection.FORWARD)
			var value: AnyRef = null
			val typeCount: Int = identTypeItems.size + stringTypeItems.size + intTypeItems.size + floatTypeItems.size
			if (typeCount != 1) {
				if (typeCount > 1) {
					logger.warn("ThingActionParam \"{}\" provided multiple values illegally and was discarded", i.getIdent)
				}
				else {
					logger.warn("ThingActionParam \"{}\" has no value and was discarded", i.getIdent)
				}
				continue //todo: continue is not supported
			}
			else {
				if (identTypeItems.size == 1) {
					value = identTypeItems.iterator.next.getIdent
				}
				else if (stringTypeItems.size == 1) {
					value = stringTypeItems.iterator.next.getValString(theParamStringValueFieldIdent, "")
				}
				else if (intTypeItems.size == 1) {
					value = intTypeItems.iterator.next.getValInteger(theParamIntValueFieldIdent, new Integer(-1))
				}
				else if (floatTypeItems.size == 1) {
					value = intTypeItems.iterator.next.getValDouble(theParamFloatValueFieldIdent, new Double(-1))
				}
			}
			if (name != null && value != null) {
				paramDictionary.putValueAtName(name, value)
			}
		}
		spec.setMyParamTVMap(paramDictionary)
	}
}
*/