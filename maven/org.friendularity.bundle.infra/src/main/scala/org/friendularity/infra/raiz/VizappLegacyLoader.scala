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

package org.friendularity.infra.raiz

import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.blob.entry.{ResourceEntryHost, EntryHost}
import org.cogchar.impl.legconf.LegacyRepoFuncs

import org.osgi.framework.BundleContext


/**
  * Created by Stub22 on 7/29/2016.
  */

trait VizappLegacyLoader extends LegacyRepoFuncs {
	def getLegacyRoots : VizappLegacyRoots

	def makeLegacyELRC (mergedProfileGraph: JenaModel) : EnhancedLocalRepoClient = {
		val legacyRoots = getLegacyRoots
		val legConfEHost = legacyRoots.getLegacyConfEHost
		val vzBrkRcpUriTxt: String = legacyRoots.getLegacyBrokerRecipeUriAbs
		makeLegacyConfigELRC_fromJena(mergedProfileGraph, vzBrkRcpUriTxt, legConfEHost)
	}

	def makeAndRegisterELRC(profileGraph : JenaModel, bunCtx : BundleContext) : EnhancedLocalRepoClient = {
		val legacyELRC = makeLegacyELRC(profileGraph)
		registerOSGiServiceForEnhRC(bunCtx, legacyELRC)
		legacyELRC
	}
}
class VizappLegacyLoaderImpl(legacyRoots : VizappLegacyRoots) extends VizappLegacyLoader {
	override def getLegacyRoots: VizappLegacyRoots = legacyRoots
}
object VizappLegacyLoaderFactory {
	val vizappRecipeNS : String = "http://onto.friendularity.org/indiv/vizappRecipes_reg_desk_2016Q1#"
	val vzpLegCnfBrkrRcpUriTxt : String = vizappRecipeNS + "vizapp_legConf_brokerRecipe"

	def makeLegacyLoader(legacyRoots : VizappLegacyRoots) : VizappLegacyLoader = new VizappLegacyLoaderImpl(legacyRoots)

	def makeLegacyLoader(legConfEHost : EntryHost, legBrkRcpUriAbs : String) : VizappLegacyLoader = {
		val legRoots = new VizappLegacyRootsImpl(legConfEHost, legBrkRcpUriAbs)
		makeLegacyLoader(legRoots)
	}
	def makeUnitTestLegacyLoader() : VizappLegacyLoader = {
		val legEHost: EntryHost = new ResourceEntryHost(classOf[VizappLegacyLoaderImpl])
		makeLegacyLoader(legEHost, vzpLegCnfBrkrRcpUriTxt)
	}

	def makeAnyOSGiLegacyLoader(bundleMarkClz : Class[_], legBrkRcpUriAbs : String) : VizappLegacyLoader = {
		val legEHost = VizappProfileLoaderFactory.makeBundleEntryHost(bundleMarkClz)
		makeLegacyLoader(legEHost, legBrkRcpUriAbs)
	}
	def makeDlftOSGiLegacyLoader(bundleMarkClz : Class[_]) : VizappLegacyLoader = makeAnyOSGiLegacyLoader(bundleMarkClz, vzpLegCnfBrkrRcpUriTxt)
}
