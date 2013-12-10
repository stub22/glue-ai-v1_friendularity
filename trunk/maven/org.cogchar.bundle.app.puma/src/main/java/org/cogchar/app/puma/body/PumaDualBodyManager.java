/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

package org.cogchar.app.puma.body;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.app.puma.config.PumaConfigManager;
import org.cogchar.app.puma.config.PumaGlobalModeManager;
import org.cogchar.name.entity.EntityRoleCN;
import org.cogchar.name.skeleton.BoneCN;
/**
 * @author Stu B. <www.texpedient.com>
 */

public class PumaDualBodyManager extends BasicDebugger {
	private		Map<Ident, PumaDualBody>	myBodies = new HashMap<Ident, PumaDualBody>();
	
	public Collection<PumaDualBody> getAllBodies() {
		return myBodies.values();
	}
	
	public void addBody(PumaDualBody pdb) {
		Ident bodyID = pdb.getCharIdent();
		myBodies.put(bodyID, pdb);
	}
	public PumaDualBody getBody(Ident bodyID) {
		return myBodies.get(bodyID);
	}
	public void disconnectAllBodies() { 
		for (PumaDualBody pdb : getAllBodies()) {
			pdb.getBodyGateway().disconnectBonyCharFromRobokindSvcs();
		}
	}
	public void clear() { 
		myBodies.clear();
	}
	public void reloadAllBoneRobotConfigs(PumaConfigManager pcm, RepoClient rc) {
		final PumaGlobalModeManager pgmm = pcm.getGlobalModeMgr();

		BoneCN bqn = new BoneCN();
		for (PumaDualBody pdb : getAllBodies()) {
			Ident charID = pdb.getCharIdent();
			getLogger().info("Updating bony config for char [" + pdb + "]");
			try {
				Ident graphIdent = pgmm.resolveGraphForCharAndRole(charID, EntityRoleCN.BONY_CONFIG_ROLE);
				try {
					pdb.updateBonyConfig(rc, graphIdent, bqn);
				} catch (Throwable t) {
					getLogger().error("problem updating bony config from queries for {}", charID, t);
				}
			} catch (Exception e) {
				getLogger().warn("Could not get a valid graph on which to query for config update of {}", charID.getLocalName());
			}
		}
	}	
}
