/*
 *  Copyright 2012 by The Friendularity Project (www.friendularity.org).
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

package org.friendu.joswrap;

import com.hp.hpl.jena.query.Dataset;
import com.hp.hpl.jena.rdf.model.Resource;
import org.friendularity.bundle.repo.Activator;

/**
 * This class is the lynchpin of our customization of Joseki
 * 
 * @author Stu B. <www.texpedient.com>
 */

public class RepoJosDsetDesc extends ModJosDatasetDesc {
	public RepoJosDsetDesc(Resource configRootRes) {
		super(configRootRes);
	}
	/**
	 * Called from super.initialize()
	 * 
	 * @return 
	 */
	@Override protected Dataset newDataset() { 
		System.out.println("newDataset() invoked for [" + datasetRoot  + "]");
		String uriFrag = datasetRoot.getLocalName();
		Dataset rcd = Activator.theMainConfigDataset;
		if ((rcd != null) && uriFrag.toLowerCase().contains("repo")) {
			System.out.println("newDataset() Returning special Friendu-Repo MainConfigDataset: " + rcd);
			return rcd;
		} else {
			System.out.println("newDataset() returning default Joseki implementation");
			return super.newDataset();
		}
	}
}
