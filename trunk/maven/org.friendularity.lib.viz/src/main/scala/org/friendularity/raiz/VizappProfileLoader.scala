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

import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.cogchar.blob.entry.{ResourceEntryHost, EntryHost}
import org.cogchar.impl.appro.ApproProfileGraphMaker
import org.cogchar.impl.legconf.UtilFuncs

/**
  * Created by Stub22 on 7/29/2016.
  */
trait VizappProfileLoader {
	protected def  getProfileRoots : VizappProfileRoots

	lazy val myApproGrphMkr = {
		val prfRoots = getProfileRoots
		new ApproProfileGraphMaker(prfRoots.getProfileEHost, prfRoots.getProfilePathRel, prfRoots.getActiveProfileTokens)
	}
	def makeMergedProfileGraph : JenaModel = myApproGrphMkr.makeMergedProfileGraph

}
class VizappProfileLoaderImpl(profileRoots: VizappProfileRoots) extends VizappProfileLoader {
	override protected def getProfileRoots: VizappProfileRoots = profileRoots
}

object VizappProfileLoaderFactory extends UtilFuncs {
	def makeProfileLoader(profileRoots: VizappProfileRoots) : VizappProfileLoader = new VizappProfileLoaderImpl(profileRoots)

	def makeUnitTestProfileLoader(profileEHost : EntryHost, profileRelFolder : String,
								  activeTokens : Array[String]) : VizappProfileLoader = {
		val profRoots = new VizappProfileRootsImpl(profileEHost, profileRelFolder, activeTokens)
		makeProfileLoader(profRoots)
	}
	def makeUnitTestProfileLoader(resMarkClz : Class[_], profileRelFolder : String,
								  activeTokens : Array[String]) : VizappProfileLoader = {
		val profileEHost: EntryHost = new ResourceEntryHost(resMarkClz)
		makeUnitTestProfileLoader(profileEHost, profileRelFolder, activeTokens)
	}
	val regDesktopTokens = Array[String]("all", "regular", "desktop")
	val vizDlftProfilePath = "org/friendu/tchunk/vizapp_profile"

	def makeUnitTestProfileLoader(): VizappProfileLoader = {
		val vizappImplClz = classOf[VizappProfileLoaderImpl]
		makeUnitTestProfileLoader(vizappImplClz, vizDlftProfilePath, regDesktopTokens)
	}

	def makeOSGiCompatProfileLoader(bundMarkClz : Class[_], profileRelFolder : String,
									activeTokens : Array[String]) : VizappProfileLoader = {
		val profileEHost = makeBundleEntryHost(bundMarkClz)
		makeUnitTestProfileLoader(profileEHost, profileRelFolder, activeTokens)
	}
}