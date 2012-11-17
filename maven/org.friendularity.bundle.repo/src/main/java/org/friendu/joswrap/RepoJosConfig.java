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

import com.hp.hpl.jena.util.FileManager;
import org.joseki.ServiceRegistry;

/**
 *  Not using this yet, just relying on the inline override at line 537 of ModJosConfig.
 * 
 * @author Stu B. <www.texpedient.com>
 * 
 * 
 */

public class RepoJosConfig extends ModJosConfiguration {
    public RepoJosConfig(FileManager fileManager, String cfgModelPath, ServiceRegistry registry) {
		super(fileManager, cfgModelPath, registry);
	}
	
	
}
