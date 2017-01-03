/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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

import com.jme3.font.BitmapText;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.cogchar.name.goody.GoodyNames;
import org.cogchar.render.app.entity.GoodyActionExtractor;
import org.cogchar.render.app.entity.VWorldEntity;
import org.cogchar.render.goody.basic.BasicGoodyCtx;

import org.cogchar.render.sys.registry.RenderRegistryClient;

import java.util.ArrayList;
import java.util.List;

/**
 * A goody to create the Cogchar ScoreBoard. Intended to completely replace original
 * org.cogchar.render.sys.physics.ScoreBoard, which can be removed entirely when no longer needed
 * for demo bundles
 * 
 * This is a "composite goody" which spawns an instance of BasicGoody2dImpl for each row of the scoreboard.
 * 
 * If we decide we need the original ScoreBoard class in the long run after all, this can be refactored as an extension
 *
 * @author Ryan Biggs <rbiggs@hansonrobokind.com>
 */

// May eventually extend a standard "composite goody" superclass
// Presents a set of lines containing "labels" and "scores".
public class TG_ScoreBoardGoody extends TG_FlatGoody implements TG_GeneralScoreBoard {

	final static ColorRGBA MY_SCORE_COLOR = ColorRGBA.Magenta; // Likely only temporarily a constant

//	int	myScreenWidth, myScreenHeight; // in pixels
	float myRowHeight; // as a fraction of window height
	Vector3f myPosition; // of top left corner of scoreboard in fraction of window width/height
	List<TG_ScoreBoardGoody.Row>	myRows;

	private	Node	myNode;

	public class Row extends TG_FlatGoodyTextElement {
		public Row(RenderRegistryClient aRenderRegCli, Ident uri, Vector3f rowPosition, float textSize, ColorRGBA scoreColor) {
			super(aRenderRegCli);
			//myLogger.info("In ScoreBoardGoody.Row, Window size is {}x{}, position is {}", new Object[]{myScreenWidth, myScreenHeight, rowPosition}); // TEST ONLY
			this.setScreenPosRelToParent(rowPosition, VWorldEntity.QueueingStyle.QUEUE_AND_RETURN);
			setContentText("_");
			setUniformScaleFactor(textSize, QueueingStyle.QUEUE_AND_RETURN);
			setColor(scoreColor);
		}
		public void setScoreText(String scoreText) {
			setContentText(scoreText);
		}
	}
	public TG_ScoreBoardGoody(BasicGoodyCtx bgc, Ident uri, Vector3f topPosition,
							  float rowHeight, int numRows, float textSize) {
		super(bgc, uri);
		myNode = new Node("ScoreBoardGoody_" + uri.getLocalName());
		myRows = new ArrayList<TG_ScoreBoardGoody.Row>();
		myRowHeight = rowHeight;

		// myPosition = topPosition;
		String baseUriString = uri.getAbsUriString();
		RenderRegistryClient grrc = bgc.getRRC();
		for (int rowIdx=0; rowIdx < numRows; rowIdx++) {
			Ident rowIdent = new FreeIdent(baseUriString + "Row" + rowIdx);
			TG_ScoreBoardGoody.Row aLine = new TG_ScoreBoardGoody.Row(grrc, rowIdent,
					getPositionForRow(rowIdx, topPosition), textSize, MY_SCORE_COLOR);
			myRows.add(aLine);
			aLine.setScoreText("line_" + rowIdx);
			// Does this node-attachment step need to be queued?
			// Seems that because the parentNode is not attached to grandParent yet, it is OK to attach kids to it.
			BitmapText rowTextNode = aLine.getTextNode();
			myNode.attachChild(rowTextNode);
		}

		setPosition(topPosition, QueueingStyle.QUEUE_AND_RETURN);
		//BonyRenderContext.setScoreBoard(this); //needs to happen somewhere, probably not here!
	}
	@Override protected Node getFlatGoodyNode() {
		return myNode;
	}
	@Override public void displayScore(int rowNum, String scoreText) {
		if ((rowNum >= 0) && (rowNum < myRows.size())) {
			TG_ScoreBoardGoody.Row l = myRows.get(rowNum);
			l.setScoreText(scoreText);
		} else {
			getLogger().warn("A request was made to set text for Scoreboard row #{}, but that row does not exist!", rowNum);
		}
	}
	
	/*
	// For this composite goody, we must attach all rows
	public void attachToVirtualWorldNode(QueueingStyle qStyle) {
		if (!myRows.isEmpty()) {
			for (Row nextRow : myRows) {
				nextRow.attachToOverlaySpatial(qStyle);
			}
		} else {
			getLogger().warn("Attempting to attach scoreboard to virtual world, but it has no rows!");
		}
	}
	
	@Override public void detachFromVirtualWorldNode(QueueingStyle qStyle) {
		for (Row nextRow : myRows) {
			nextRow.detachFromVirtualWorldNode(qStyle);
		}
	}
	*/
	@Override public void applyAction(GoodyActionExtractor ga, QueueingStyle qStyle) {
		switch (ga.getKind()) {
			case MOVE : {
				setPosition(ga.getLocationVec3f(), qStyle);
				break;
			}
			case SET : {
				String scoreText = ga.getSpecialString(GoodyNames.TEXT);
				int rowNum = 0;
				try {
					rowNum = Integer.valueOf(ga.getSpecialString(GoodyNames.SUBCOMPONENT));
				} catch (Exception e) {
					getLogger().error("Row (subcomponent) number not recognized for setting scoreboard, assuming 0");
				}
				displayScore(rowNum, scoreText);
				break;
			}
			default: {
				getLogger().error("Unknown action requested in Goody {}: {}", getUri().getLocalName(), ga.getKind().name());
			}
		}
	};
	
	@Override public void setPosition(Vector3f position, QueueingStyle qStyle) {
		super.setPosition(position, qStyle);
		for (int rowIdx=0; rowIdx < myRows.size(); rowIdx++) {
			Row nextRow = myRows.get(rowIdx);
			nextRow.setScreenPosRelToParent(getPositionForRow(rowIdx, position), qStyle);
		}
		myPosition = position;
	}
	
	@Override public void setUniformScaleFactor(Float scale, QueueingStyle qStyle) {
		getLogger().warn("Setting scale not currently implemented for the ScoreBoardGoody, coming soon...");
		/*// This throws a java.lang.IllegalArgumentException deep in jME's LWJGL bits for some reason
		// Will fix this soon, not very necessary for now and holding up a big commit...
		if (scale != null) {
			for (Row nextRow : myRows) {
				nextRow.setScale(scale);
			}
		}
		*/
	}
	
	private Vector3f getPositionForRow(int row, Vector3f scoreboardPosition) {
		float leftX = scoreboardPosition.getX();
		float topY = scoreboardPosition.getY() - row * myRowHeight;
		return new Vector3f(leftX, topY, 0);
	}
	@Override public void setRotation(Quaternion newRotation, QueueingStyle qStyle) {
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}	
}
