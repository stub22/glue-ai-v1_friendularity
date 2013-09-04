/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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
package org.friendularity.impl.visual;

import org.cogchar.render.sys.registry.RenderRegistryClient;
import com.jme3.asset.AssetManager;
import com.jme3.scene.Node;

import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.blob.emit.RenderConfigEmitter;
import org.friendularity.api.west.EstimateVisualizer;
import org.friendularity.api.west.ThingEstimate;

import org.friendularity.api.west.WorldEstimate;
import org.friendularity.vworld.MeshTest;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class BonusVisualizer extends ShapeAnimVisualizer<WorldEstimate> {


	public BonusVisualizer(HumanoidRenderContext hrc) {
		super(hrc);
	}


	public void makeBonusMeshes() {
		RenderRegistryClient rrc = getRenderRegistryClient();
		AssetManager amgr = rrc.getJme3AssetManager(null);
		Node rootNode = rrc.getJme3RootDeepNode(null);
		RenderConfigEmitter rce = getConfigEmitter();
		MeshTest mt = new MeshTest();
		mt.makeStuff(amgr, rootNode);
	}



}
