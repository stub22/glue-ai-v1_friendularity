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

import org.cogchar.blob.entry.EntryHost

/**
  * Created by Stub22 on 7/29/2016.
  */

trait VizappProfileRoots {
	def getProfileEHost : EntryHost

	def getProfilePathRel : String

	def getActiveProfileTokens : Array[String]

}
case class VizappProfileRootsImpl(profileEHost : EntryHost, profilePathRel : String,
								  activeProfileTokens : Array[String]) extends VizappProfileRoots {
	override def getProfileEHost : EntryHost = profileEHost

	override def getProfilePathRel : String = profilePathRel

	override def getActiveProfileTokens : Array[String] = activeProfileTokens
}
trait VizappLegacyRoots {
	def getLegacyConfEHost : EntryHost

	def getLegacyBrokerRecipeUriAbs : String  // Leads to query-sheet props in profile-recipes

	//def getLegacyConfPathRel : String  // Comes from profile-recipe

}
case class VizappLegacyRootsImpl(legConfEHost : EntryHost, legBrkRcpUriAbs : String) extends VizappLegacyRoots {
	def getLegacyConfEHost : EntryHost = legConfEHost

	def getLegacyBrokerRecipeUriAbs : String  = legBrkRcpUriAbs


}
case class VizappRootsGroup(profileRoots : VizappProfileRoots, legacyRoots : VizappLegacyRoots) {
	def getProfileRoots : VizappProfileRoots = profileRoots
	def getLegacyRoots : VizappLegacyRoots = legacyRoots
}

/*
trait VizappTestRootNames {
	private val vizappRecipeNS : String = "http://onto.friendularity.org/indiv/vizappRecipes_reg_desk_2016Q1#"
	val vzpLegCnfBrkrRcpUriTxt : String = vizappRecipeNS + "vizapp_legConf_brokerRecipe"
	val pathToProfileFolder : String = "org/friendu/tchunk/vizapp_profile" // relative to profile eHost
}
class VizappRaizNamesImpl(profileFolder : String ) extends VizappTestRootNames {
	override val pathToProfileFolder = profileFolder
}
*/