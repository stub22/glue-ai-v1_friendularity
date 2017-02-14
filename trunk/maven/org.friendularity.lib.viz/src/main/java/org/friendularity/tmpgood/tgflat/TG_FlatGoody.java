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

package org.friendularity.tmpgood.tgflat;

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import org.appdapter.core.name.Ident;
import org.cogchar.render.app.entity.GoodyActionExtractor;
import org.cogchar.render.goody.basic.BasicGoodyCtx;
import org.cogchar.render.goody.basic.BasicVWorldEntity;
import org.cogchar.render.opengl.scene.FlatOverlayMgr;

import java.util.concurrent.Callable;
import scala.Option;


/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class TG_FlatGoody extends BasicVWorldEntity {
	private FlatOverlayMgr myOverlayMgr;
	private	ColorRGBA		myForeColor;

	protected TG_FlatGoody(BasicGoodyCtx bgc, Ident uri) {
		super (bgc, uri);
		myOverlayMgr =bgc.getRRC().getSceneFlatFacade(null);
	}
	protected abstract Node getFlatGoodyNode();
	@Override public void attachToVirtualWorldNode(Node vWorldNode, QueueingStyle qStyle) {
		// Currently any specified node is ignored since we are attaching via the FlatOverlayMgr
		attachToOverlaySpatial(qStyle);
	}
	protected void attachToOverlaySpatial(QueueingStyle style) {
		final Node fgn = getFlatGoodyNode();
		if (fgn != null) {
			// Location may be incorrect at this time, because location change may be delayed by queueing to JME thread.
			enqueueForJme(new Callable() { // Do this on main render thread
				@Override public Void call() throws Exception {
					getLogger().debug("Attaching 2d goody to virtual world: name={}, location={}, gnode={}", getUri().getLocalName(),
							fgn.getLocalTranslation(), fgn);
					myOverlayMgr.attachOverlaySpatial(fgn);
					return null;
				}
			}, style);
		} else {
			getLogger().warn("Attempting to attach 2D Goody {} to virtual world, but no FlatGoody Node was found",
					getUri().getLocalName());
		}
	}

	@Override public void detachFromVirtualWorldNode(QueueingStyle style) {
		final Node fgn = getFlatGoodyNode();
		enqueueForJme(new Callable() { // Do this on main render thread

				@Override public Void call() throws Exception {
					myOverlayMgr.detachOverlaySpatial(fgn);
					return null;
				}
			}, style);
	}
	// Usually we want wait = true, but not for repositioning during window size change
	protected void setScreenPosition(final Vector3f screenPos, QueueingStyle qStyle) {
		final Node fgn = getFlatGoodyNode();
		//myLogger.info("Setting position: {}", position); // TEST ONLY
		if (fgn != null) {
			Callable positioningCallable = new Callable() { // Do this on main render thread
				@Override public Void call() throws Exception {
					getLogger().debug("Setting screen position for {} to {}", getUri().getLocalName(), 	screenPos);
					fgn.setLocalTranslation(screenPos);
					return null;
				}
			};
			enqueueForJme(positioningCallable, qStyle);
		}
	}
	// Override this method to add functionality; be sure to call this super method to apply standard Goody actions
	@Override	public void applyAction(GoodyActionExtractor ga, QueueingStyle qStyle) {
		switch (ga.getKind()) {
			case MOVE :
			case SET : {
				Vector3f locVec = ga.getLocationVec3f();
				if (locVec != null) {
					setScreenPosition(locVec, qStyle);
				}
				setUniformScaleFactor(ga.getScaleUniform(), qStyle);
				Option<ColorRGBA> c = ga.getColor();
				if (c.isDefined()) {
					myForeColor = ga.getColorOrDefault();
				}
				break;
			}
			default: {
				getLogger().error("Unknown action requested in Goody {}: {}", getUri().getLocalName(), ga.getKind().name());
			}
		}
	}
	@Override public void setPosition(Vector3f position, QueueingStyle style) {
		// getLogger().error("setPosition to {} is not implemented", position);
		setScreenPosition(position, style);
	}

}
