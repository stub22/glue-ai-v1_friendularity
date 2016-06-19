package org.friendularity.chnkr

import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.{RepoClient, EnhancedRepoClient, EnhancedLocalRepoClient};
import org.appdapter.fancy.rspec.RepoSpec;
import org.appdapter.core.store.Repo
import org.cogchar.blob.entry.{BundleEntryHost, EntryHost}
;
import org.cogchar.impl.scene.read.BehavMasterConfigTest;

import org.cogchar.api.owrap.crcp.{BRFeature => CC_BRFeature}
import org.cogchar.api.owrap.appro.{AFBRLegacyConfig}
import com.hp.hpl.jena
import org.apache.jena.riot.RDFDataMgr

import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }
import jena.ontology.Individual

import org.ontoware.rdf2go
import org.ontoware.rdfreactor
import org.osgi.framework.{BundleContext, FrameworkUtil, Bundle}

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}

import org.appdapter.fancy.log.VarargsLogging

trait UtilFuncs extends VarargsLogging {
	def open4R2go(jmodel : JenaModel) : R2GoModel = {
		val r2goModel : R2GoModel = new rdf2go.impl.jena.ModelImplJena(jmodel)
		r2goModel.open
		r2goModel
	}
	def makeBundleEntryHost (markerClz : Class[_]) : EntryHost = {
		val bun : Bundle = FrameworkUtil.getBundle(markerClz);
		if (bun == null) {
			throw new RuntimeException("Cannot locate bundle for markerClz=" + markerClz)
		}
		new BundleEntryHost(bun) ;
	}

}
trait LegacyRepoFuncs extends UtilFuncs {
	def makeLegacyConfigELRC_fromJena(recipesJM : JenaModel, cbrUri : String, cdatEH : EntryHost) : EnhancedLocalRepoClient = {
		val recipesR2Go = open4R2go(recipesJM)
		makeLegacyConfigELRC_fromR2Go(recipesR2Go, cbrUri, cdatEH)
	}
	def makeLegacyConfigELRC_fromR2Go(recipesR2Go : R2GoModel, cbrUri : String, cdatEH : EntryHost) : EnhancedLocalRepoClient = {
		val configBR = new AFBRLegacyConfig(recipesR2Go, cbrUri,  false)
		makeLegacyConfigELRC_fromRcp(configBR, cdatEH)

	}

	def makeLegacyConfRepoSpec(profileJM : JenaModel, vizappBrokerRecipeUriTxt : String,
							   cdatEH : EntryHost) :  ChnkrWrapRepoSpec = {

		val bootRecipesR2Go = open4R2go(profileJM)

		val legConfigBR = new AFBRLegacyConfig(bootRecipesR2Go, vizappBrokerRecipeUriTxt, false)

		makeLegacyConfRepoSpec(legConfigBR, cdatEH)
	}
	def makeLegacyConfRepoSpec(configBR : AFBRLegacyConfig, cdatEH : EntryHost) : ChnkrWrapRepoSpec = {
		val brokerRecipeWrap: LegacyConfBrokerRecipeWrap = new LegacyConfBrokerRecipeWrap(configBR)
		val cwRepoSpec = new ChnkrWrapRepoSpec(brokerRecipeWrap, cdatEH)
		cwRepoSpec
	}
	def makeLegacyRepoELRC_fromSpec(repoSpec : RepoSpec,
									tgtGraphSparqlVarName : String, qrySrcGraphQName : String): EnhancedLocalRepoClient = {

		val repoHandle : Repo.WithDirectory = repoSpec.getOrMakeRepo();

		val erc = new EnhancedLocalRepoClient(repoSpec, repoHandle, tgtGraphSparqlVarName, qrySrcGraphQName)
		erc
	}
	def makeLegacyConfigELRC_fromRcp(legCnfBR : AFBRLegacyConfig, // CC_BRFeature,
									 cdatEH : EntryHost) : EnhancedLocalRepoClient = {
		val cwRepoSpec = makeLegacyConfRepoSpec(legCnfBR, cdatEH)
		// Old way was:  hardcoded query setup param Strings
		// CCMIO: 2016-04-17   tgtGraphVarName=qGraph  qrySrcGraphQName=ccrt:qry_sheet_77
		// val tgtGraphSparqlVarName  = BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR
		// val qrySrcGraphQName = BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN
		// info2("Setting up legacy avatar config with tgtGraphVarName={}  qrySrcGraphQName={}",
		//			tgtGraphSparqlVarName, qrySrcGraphQName)


		val recipeSparqlTgtVarName = legCnfBR.getLegacyDefaultSparqlVarName
		// On a wrong value, first error is from FancyRepo.scala method checkQueryText
		//  "Unable to find Query Called ccrt:template_globalmode_99 in Model wrongQrySrcQName"
		// See stack trace in comments at bottom of this file.
		val recipeSparqlQrySrcQN = legCnfBR.getLegacyQuerySourceQName
		info3("Found legacy avatar query names in recipe={}  tgtGraphVarName={}  qrySrcGraphQName={}",
			legCnfBR, recipeSparqlTgtVarName, recipeSparqlQrySrcQN)

		val erc = makeLegacyRepoELRC_fromSpec(cwRepoSpec, recipeSparqlTgtVarName, recipeSparqlQrySrcQN)
		erc

	}
	def registerOSGiServiceForEnhRC(bunCtx : BundleContext,
									configERC : EnhancedRepoClient)  : Unit = {
		info1("Registering legacy config EnhancedRepoClient: {}", configERC);

		bunCtx.registerService(classOf[RepoClient].getName(), configERC, null);
	}

}