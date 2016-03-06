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

import org.cogchar.api.owrap.crcp.{BRFeature}

import com.hp.hpl.jena
import org.apache.jena.riot.RDFDataMgr

import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }
import jena.ontology.Individual
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
	private def maybeRegisterLegacyConf() : Unit = {
		val bun = org.osgi.framework.FrameworkUtil.getBundle(getClass)
		if (bun != null) {
			val bunCtx = bun.getBundleContext
			
			// val lsysRecipeNames : LessonSysRecipeNames = myLSysCtx.getMainRecipeNames

			val legConfBRURI = "confBrokerRecipeURI" //  lsysRecipeNames.getLegacyConfBrokerRecipeURI
			val legConfEHost = null //  myLSysCtx.getInLegConfEntHost
			if ((legConfBRURI != null) && (legConfEHost != null)) {
				val recipeModel = null //  myLSysCtx.getInRootRecipeModel
				val rcpmR2Go = open4R2go(recipeModel)
				makeAndRegisterAvatarConfigRepo(bunCtx, rcpmR2Go, legConfBRURI, legConfEHost)
			} else {
				warn2("Not loading legacy config for brokerRecipeURI={}, eHost={}", legConfBRURI, legConfEHost)
			}
		} else {
			warn0("OSGi bundle lookup failed (we are probably in a main() test), so no legacy config will be processed.")
		}
	}
	
	def makeAndRegisterAvatarConfigRepo( bunCtx : BundleContext, recipesR2Go : R2GoModel, cbrUri : String, cdatEH : EntryHost) : Unit = {
		val configBR : BRFeature = new BRFeature(recipesR2Go, cbrUri,  false)
		makeAndRegisterAvatarConfigRepo(bunCtx, configBR, cdatEH)
	}
	def makeAndRegisterAvatarConfigRepo( bunCtx : BundleContext, configBR : BRFeature, cdatEH : EntryHost) : Unit = {
		val cwRepoSpec = makeVWConfRepoSpec(configBR, cdatEH)
		makeAndRegisterAvatarConfigRC(bunCtx, cwRepoSpec)
	}
	def makeVWConfRepoSpec(configBR : BRFeature, cdatEH : EntryHost) : ChnkrWrapRepoSpec = {
		val brokerRecipeWrap: VWConfBrokerRecipeWrap = new VWConfBrokerRecipeWrap(configBR)
		val cwRepoSpec = new ChnkrWrapRepoSpec(brokerRecipeWrap, cdatEH)
		cwRepoSpec
	}
	private def makeAndRegisterAvatarConfigRC( bunCtx : BundleContext, repoSpec : RepoSpec ) {
		val repoHandle : Repo.WithDirectory = repoSpec.getOrMakeRepo();
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
}
/*

:tdatabun_vworld_config_broker rdf:type mrcp:MFBR_Config ,
                                        owl:NamedIndividual ;
                               
	   <http://onto.cogchar.org/onto/201502/CircusRecipe_OWL2#hasInGhostRecipe> :tdatabun_vworld_config_folderRecipe .

:tdatabun_vworld_config_folderRecipe rdf:type <http://onto.cogchar.org/onto/201502/CircusRecipe_OWL2#GhR_Refer> ,
                                              owl:NamedIndividual ;
                                     
     <http://onto.cogchar.org/onto/201502/CircusRecipe_OWL2#hasGraphHost> :tdatabun_vworld_config_folderGHost4 .

:tdatabun_vworld_config_folderGHost4 rdf:type <http://onto.cogchar.org/onto/201407/MetaDir_OWL2#GH4S_Folder> ,
                                              owl:NamedIndividual ;
                                     
     <http://onto.cogchar.org/onto/201407/MetaDir_OWL2#hasUrlText> "vwtest/data/config/grumpyvw" .

 */