/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
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


import com.jme3.asset.AssetManager;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.Rectangle;
import com.jme3.math.ColorRGBA;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;


import java.util.ArrayList;
import java.util.List;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class TG_ScoreBoard implements TG_GeneralScoreBoard {
	// Presents a set of lines containing "labels" and "scores".
	Node		myParentNode;
	BitmapFont	myLabelFont;
	BitmapFont	myScoreFont;
	ColorRGBA	myScoreColor;
	int			myBaseX, myBaseY, myWidth, myHeight;
	List<Row>	myRows;

	public class Row {
		private	BitmapText		myRenderedText;
		private	Rectangle		myRectangle;
		public Row(int baseX, int baseY, int width, int height, float textSizeMult) {
			myRenderedText = new BitmapText(myScoreFont, false);
			// This part is just responsible for clipping rectangle, not screen pos?
			// myRectangle = new Rectangle (baseX, baseY, width, height);
			// myRenderedText.setBox(myRectangle);
			float scoreFontSize = myScoreFont.getPreferredSize();
			myRenderedText.setSize(scoreFontSize * textSizeMult);
			myRenderedText.setText("_");
			myRenderedText.setLocalTranslation(baseX, baseY + height, 0);
			myRenderedText.setColor(myScoreColor);
		}
		Spatial getRenderingSpatial() {
			return myRenderedText;
		}
		public void setScoreText(String scoreText) {
			myRenderedText.setText(scoreText);
		}
	}
	public TG_ScoreBoard(AssetManager assetManager, Node parentNode, int baseX, int baseY, int boardWidth,
						 int rowHeight, int numRows, float textSizeMult) {
		myRows = new ArrayList<Row>();

		myScoreFont = assetManager.loadFont("Interface/Fonts/Default.fnt");
		myScoreColor = ColorRGBA.Magenta;

		int topY = baseY + (numRows - 1) * rowHeight;
		for (int rowIdx=0; rowIdx < numRows; rowIdx++) {
			Row aLine = new Row (baseX, topY - rowIdx * rowHeight, boardWidth, rowHeight, textSizeMult);
			parentNode.attachChild(aLine.getRenderingSpatial());
			myRows.add(aLine);
			aLine.setScoreText("line_" + rowIdx);
		}
	}
	@Override
	public void displayScore(int rowNum, String scoreText) {
		Row l = myRows.get(rowNum);
		l.setScoreText(scoreText);
	}
}
