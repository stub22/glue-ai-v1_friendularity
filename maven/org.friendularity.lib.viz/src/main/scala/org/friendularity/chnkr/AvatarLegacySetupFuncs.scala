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

package org.friendularity.chnkr
import org.appdapter.fancy.rclient.{RepoClient, EnhancedRepoClient, EnhancedLocalRepoClient};
import org.appdapter.fancy.rspec.RepoSpec;
import org.appdapter.core.store.Repo;
import org.cogchar.impl.scene.read.BehavMasterConfigTest;

import org.cogchar.api.owrap.crcp.{BRFeature => CC_BRFeature}
import org.cogchar.api.owrap.appro.{AFBRLegacyConfig}
import com.hp.hpl.jena
import org.apache.jena.riot.RDFDataMgr

import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }
import jena.ontology.Individual
import org.friendularity.appro.{ApproRaizCtx, ApproRaizRecipeNames}
import org.ontoware.rdf2go
import org.ontoware.rdfreactor

import rdf2go.model.{Model => R2GoModel}
import rdf2go.model.node.{URI => R2GoURI}

import org.appdapter.fancy.log.VarargsLogging

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
	
import org.cogchar.blob.entry.EntryHost;
import org.cogchar.blob.entry.BundleEntryHost;
// registerAvatarConfigRepoClient(bunCtx, erc)
trait AvatarLegacySetupFuncs extends VarargsLogging {

	def makeAvatarLegacyConfigRepo(recipesJM : JenaModel, cbrUri : String, cdatEH : EntryHost) : EnhancedLocalRepoClient = {
		val recipesR2Go = open4R2go(recipesJM)
		makeAvatarLegacyConfigRepo(recipesR2Go, cbrUri, cdatEH)
	}
	def makeAvatarLegacyConfigRepo(recipesR2Go : R2GoModel, cbrUri : String, cdatEH : EntryHost) : EnhancedLocalRepoClient = {
		val configBR = new AFBRLegacyConfig(recipesR2Go, cbrUri,  false)
		makeAvatarLegacyConfigRepo(configBR, cdatEH)

	}
	def makeAvatarLegacyConfigRepo(legCnfBR : AFBRLegacyConfig, // CC_BRFeature,
										cdatEH : EntryHost) : EnhancedLocalRepoClient = {
		val cwRepoSpec = makeVWConfRepoSpec(legCnfBR, cdatEH)
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

		val erc = makeAvatarLegacyConfigERC(cwRepoSpec, recipeSparqlTgtVarName, recipeSparqlQrySrcQN)
		erc


	}
	def makeVWConfRepoSpec(profileJM : JenaModel, vizappBrokerRecipeUriTxt : String,
						   cdatEH : EntryHost) :  ChnkrWrapRepoSpec = {

		val bootRecipesR2Go = open4R2go(profileJM)

		val legConfigBR = new AFBRLegacyConfig(bootRecipesR2Go, vizappBrokerRecipeUriTxt, false)

		makeVWConfRepoSpec(legConfigBR, cdatEH)
	}
	def makeVWConfRepoSpec(configBR : AFBRLegacyConfig, cdatEH : EntryHost) : ChnkrWrapRepoSpec = {
		val brokerRecipeWrap: LegacyConfBrokerRecipeWrap = new LegacyConfBrokerRecipeWrap(configBR)
		val cwRepoSpec = new ChnkrWrapRepoSpec(brokerRecipeWrap, cdatEH)
		cwRepoSpec
	}
	def makeAvatarLegacyConfigERC( repoSpec : RepoSpec,
					tgtGraphSparqlVarName : String, qrySrcGraphQName : String): EnhancedLocalRepoClient = {

		val repoHandle : Repo.WithDirectory = repoSpec.getOrMakeRepo();

		val erc = new EnhancedLocalRepoClient(repoSpec, repoHandle, tgtGraphSparqlVarName, qrySrcGraphQName)
		erc
	}

	def registerAvatarConfigRepoClient(bunCtx : BundleContext,
							avatarConfigERC : EnhancedRepoClient)  : Unit = {
		info1("Registering legacy config EnhancedRepoClient: {}", avatarConfigERC);
		
		bunCtx.registerService(classOf[RepoClient].getName(), avatarConfigERC, null);
	}
	private def open4R2go(jmodel : JenaModel) : R2GoModel = {
		val r2goModel : R2GoModel = new rdf2go.impl.jena.ModelImplJena(jmodel)
		r2goModel.open
		r2goModel
	}
	def makeBundleEntryHost (markerClz : Class[_]) : EntryHost = {
		val bun : Bundle = FrameworkUtil.getBundle(markerClz);
		new BundleEntryHost(bun) ;
	}
}
/*
	def loadVizappFromChunkFolders(profilePath : String, vwrldConfPath: String): Unit = {

	}

	private def maybeRegisterLegacyConf(arzCtx : ApproRaizCtx) : Unit = {
		val bun = org.osgi.framework.FrameworkUtil.getBundle(getClass)
		if (bun != null) {
			val bunCtx = bun.getBundleContext

			val arzRecipeNames : ApproRaizRecipeNames = arzCtx.getMainRecipeNames

			val legConfBRURI = arzRecipeNames.getLegacyConfBrokerRecipeURI
			val legConfEHost = arzCtx.getInLegConfEntHost
			if ((legConfBRURI != null) && (legConfEHost != null)) {
				val recipeModel = arzCtx.getInRootRecipeModel
				val rcpmR2Go = open4R2go(recipeModel)
				makeAndRegisterAvatarConfigRepo(bunCtx, rcpmR2Go, legConfBRURI, legConfEHost)
			} else {
				warn2("Not loading legacy config for brokerRecipeURI={}, eHost={}", legConfBRURI, legConfEHost)
			}
		} else {
			warn0("OSGi bundle lookup failed (we are probably in a main() test), so no legacy config will be processed.")
		}
	}

	BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR, BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN);
	 final val TGT_GRAPH_SPARQL_VAR = RepoSpecDefaultNames.DFLT_TGT_GRAPH_SPARQL_VAR; // "qGraph"
  val QUERY_SOURCE_GRAPH_QN = MasterDemoNames.QUERY_SOURCE_GRAPH_QN;



Comments below copied from org.appdapter.fancy.rspec.RepoSpecDefaultNames on 2016-04-17

	  // These 2 string constants establish repo-client wrapper defaults, giving a default
  // query context to easily fetch from.
  // Either value may be bypassed/overidden using either
  //      A) Overrides of the RepoSpec methods below
  //   or B) The more general forms of queryIndirect_.
  // 1) Default query *source* graph QName used in directory model (Sheet or RDF).
  // We read SPARQL text from this graph, which we use to query *other* graphs.
  // This graph is typically not used as a regular data graph by  other low-order
  // query operations, although there is no prohibition or protection from doing so
  // at this time.   This query source graph may be overridden using the more general
  // forms of queryIndirect_.

  val DFLT_QRY_SRC_GRAPH_TYPE = "ccrt:qry_sheet_22"

  // 2) default variable name for a single target graph in a SPARQL query.
  // This is used in the convenience forms of queryIndirect that handle many common
  // use cases, wherein the query needs a single graph to operate on that is switched
  // by application logic or user selection.

  val DFLT_TGT_GRAPH_SPARQL_VAR = "qGraph"


  ...BUT, then in Cogchar we have  MasterDemoNames which appears to be the QuerySource
   name actually used at present.

   Fortunately we note that our qry_sheet_77 and qry_sheet_22 are identical in contents.

   {

	  val QUERY_SOURCE_GRAPH_QN : String = "ccrt:qry_sheet_77"
	  val CHAN_BIND_GRAPH_QN : String = "csi:chan_sheet_77"

	  val GROUP_KEY_CHAN_BIND : String = "ChannelBindingGroupId"
	  val CHAN_GROUP_QN : String = "csi:dm_chan_group_22"

	  val DIRECT_BEHAV_GRAPH_QN : String = "csi:behavScene_sheet_77"

	  val BEHAV_STEP_GRAPH_QN : String = "csi:behavStep_sheet_77"
	  val BEHAV_SCENE_GRAPH_QN : String = "csi:behavScene_sheet_77"
	  val DERIVED_BEHAV_GRAPH_QN : String = "csi:merged_model_5001"

	  val PIPELINE_GRAPH_QN : String = "csi:pipeline_sheet_77"
	  val PIPE_QUERY_QN : String = "ccrt:find_pipes_77"
	  val PIPE_SOURCE_QUERY_QN : String = "ccrt:find_pipe_sources_78"


	  val GROUP_KEY_SCENE_SPEC : String = "SceneSpecGroupId"
	  val SCENE_GROUP_QN : String = "csi:scene_group_33"


	  val GROUP_KEY_THEATER : String = "TheaterGroupId"
	  val THEATER_GROUP_QN : String = "csi:theater_group_44"


}

     [java] java.lang.RuntimeException: Unable to find Query Called ccrt:template_globalmode_99 in Model wrongQrySrcQName
     [java] 	at org.appdapter.fancy.repo.FancyRepo$class.checkQueryText(FancyRepo.scala:153)
     [java] 	at org.friendularity.chnkr.ChnkrWrapRepo.checkQueryText(ChnkrWrapRepo.scala:30)
     [java] 	at org.appdapter.fancy.repo.FancyRepo$class.queryIndirectForAllSolutions(FancyRepo.scala:147)
     [java] 	at org.friendularity.chnkr.ChnkrWrapRepo.queryIndirectForAllSolutions(ChnkrWrapRepo.scala:30)
     [java] 	at org.appdapter.fancy.rclient.LocalRepoClientImpl.queryIndirectForAllSolutions(LocalRepoClientImpl.scala:98)
     [java] 	at org.appdapter.fancy.rclient.RepoClientImpl.queryIndirectForAllSolutions(RepoClientImpl.scala:56)
     [java] 	at org.appdapter.fancy.rclient.RepoClientImpl.queryIndirectForAllSolutions(RepoClientImpl.scala:62)
     [java] 	at org.cogchar.blob.emit.GlobalConfigEmitter.<init>(GlobalConfigEmitter.scala:83)
     [java] 	at org.cogchar.app.puma.config.PumaGlobalModeManager.applyGlobalConfig(PumaGlobalModeManager.java:57)
     [java] 	at org.cogchar.app.puma.boot.PumaAppContext.startRepositoryConfigServices(PumaAppContext.java:150)
     [java] 	at org.cogchar.app.puma.boot.PumaBooter.pumaBootUnsafeUnderOSGi(PumaBooter.java:133)
     [java] 	at org.cogchar.app.puma.boot.PumaBooter.bootUnderOSGi(PumaBooter.java:83)
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.launchPumaRobotsAndChars(CCMIO_DemoActivator.java:151)
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.launchCcmioDemo(CCMIO_DemoActivator.java:114)
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.handleFrameworkStartedEvent(CCMIO_DemoActivator.java:104)
 */