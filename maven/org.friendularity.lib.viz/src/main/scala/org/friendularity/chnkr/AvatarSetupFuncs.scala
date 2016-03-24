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

trait AvatarSetupFuncs extends VarargsLogging {

	def makeAndRegisterAvatarConfigRepo( bunCtx : BundleContext, recipesR2Go : R2GoModel, cbrUri : String, cdatEH : EntryHost) : Unit = {
		val configBR = new CC_BRFeature(recipesR2Go, cbrUri,  false)
		makeAndRegisterAvatarConfigRepo(bunCtx, configBR, cdatEH)
	}
	def makeAndRegisterAvatarConfigRepo( bunCtx : BundleContext, configBR : CC_BRFeature, cdatEH : EntryHost) : Unit = {
		val cwRepoSpec = makeVWConfRepoSpec(configBR, cdatEH)
		makeAndRegisterAvatarConfigRC(bunCtx, cwRepoSpec)
	}
	def makeVWConfRepoSpec(profileJM : JenaModel, vizappBrokerRecipeUriTxt : String, cdatEH : EntryHost) :  ChnkrWrapRepoSpec = {

		val bootRecipesR2Go = open4R2go(profileJM)

		val legConfigBR = new CC_BRFeature(bootRecipesR2Go, vizappBrokerRecipeUriTxt, false)

		makeVWConfRepoSpec(legConfigBR, cdatEH)
	}
	def makeVWConfRepoSpec(configBR : CC_BRFeature, cdatEH : EntryHost) : ChnkrWrapRepoSpec = {
		val brokerRecipeWrap: VWConfBrokerRecipeWrap = new VWConfBrokerRecipeWrap(configBR)
		val cwRepoSpec = new ChnkrWrapRepoSpec(brokerRecipeWrap, cdatEH)
		cwRepoSpec
	}
	def makeAndRegisterAvatarConfigRC( bunCtx : BundleContext, repoSpec : RepoSpec ) {
		val repoHandle : Repo.WithDirectory = repoSpec.getOrMakeRepo();
		// TODO: Get these SPARQL keys from profile, instead
		val erc = new EnhancedLocalRepoClient(repoSpec, repoHandle, 
				BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR, BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN);
		registerAvatarConfigRepoClient(bunCtx, erc);
	}

	private def registerAvatarConfigRepoClient(bunCtx : BundleContext, avatarConfigERC : EnhancedRepoClient)  : Unit = {
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
 */