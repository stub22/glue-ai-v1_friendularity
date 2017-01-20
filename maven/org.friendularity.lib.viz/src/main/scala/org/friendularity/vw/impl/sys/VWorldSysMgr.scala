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

package org.friendularity.vw.impl.sys


/**
  * Created by Stub22 on 4/14/2016.
  */

// Legit state of a running VWorld system is managed by an instance of VWorldSysMgr.
trait VWorldSysMgr {
	// See VWPTRendezvous
	// def findPublicTellers : VWorldPublicTellers

//	def findGoodyCtx_opt : Option[BasicGoodyCtx] = None
}
// (hack)Strap holds any icky extra shared state that we temporarily want to pass to VWorldBossActor.
trait VWorldStrap {
//	def	getPumpCtx : DullPumpCtx
}




class VWSysMgrImpl extends VWorldSysMgr {

	// override def findPublicTellers : VWorldPublicTellers = new VWorldPublicTellers{}

}


class VWStrapImpl extends VWorldStrap {
	// Good news - the hackStrap is still empty!  Let's hope it stays that way.
}


