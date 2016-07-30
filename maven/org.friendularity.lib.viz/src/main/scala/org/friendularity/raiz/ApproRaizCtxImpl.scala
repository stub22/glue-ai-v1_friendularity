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

package org.friendularity.raiz

import com.hp.hpl.jena
import jena.rdf.model.{ Model => JenaModel, ModelFactory => JenaModelFactory }
import jena.ontology.Individual
import org.cogchar.blob.entry.{EntryHost}

/*
As of 2016-07-29, the ApproRaizCtx stuff is not used.
 */

trait ApproRaizCtx {
	def getInRootRecipeModel: JenaModel
	def getInRootSettingsModel: JenaModel
	def getOutStatusModel: JenaModel

	def getInLegConfEntHost: EntryHost

	def getMainRecipeNames: ApproRaizRecipeNames
}

class ApproRaizCtxImpl(rootRecipeModel: JenaModel, rootSettingsModel: JenaModel,
					   statusOutModel: JenaModel, legConfEntHost: EntryHost,
					   mainRecipeNames: ApproRaizRecipeNames) extends ApproRaizCtx {

	override def getInRootRecipeModel: JenaModel = rootRecipeModel
	override def getInRootSettingsModel: JenaModel = rootSettingsModel
	override def getOutStatusModel: JenaModel = statusOutModel

	override def getInLegConfEntHost: EntryHost = legConfEntHost

	override def getMainRecipeNames: ApproRaizRecipeNames = mainRecipeNames
}

trait ApproRaizRecipeNames {
	def getLegacyConfBrokerRecipeURI: String
}

class ApproRaizRecipeNamesImpl(legConfBRURI: String) extends ApproRaizRecipeNames {
	override def getLegacyConfBrokerRecipeURI: String = legConfBRURI
}
