/*
 *  Copyright 2011 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity;


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

public class ScoreBoard {
	// Presents a set of lines containing "labels" and "scores".
	Node		myParentNode;
	BitmapFont	myLabelFont;
	BitmapFont	myScoreFont;
	ColorRGBA	myScoreColor;
	int			myBaseX, myBaseY, myWidth, myHeight;
	List<Line>	myLines;

	public class Line {
		private	BitmapText		myRenderedText;
		private	Rectangle		myRectangle;
		public Line(int baseX, int baseY, int width, int height) {
			myRenderedText = new BitmapText(myScoreFont, false);
			myRectangle = new Rectangle (baseX, baseY, width, height);
			myRenderedText.setBox(myRectangle);
			float scoreFontSize = myScoreFont.getPreferredSize();
			myRenderedText.setSize(scoreFontSize * 2f);
			myRenderedText.setText("_");
			myRenderedText.setLocalTranslation(0, height, 0);
			myRenderedText.setColor(myScoreColor);
		}
		Spatial getRenderingSpatial() {
			return myRenderedText;
		}
		public void setScoreText(String scoreText) {
			myRenderedText.setText(scoreText);
		}
	}
	public ScoreBoard(AssetManager assetManager, Node parentNode, int numLines, int baseX, int baseY, int width, int height) {
		myLines = new ArrayList<Line>();

		myScoreFont = assetManager.loadFont("Interface/Fonts/Default.fnt");
		myScoreColor = ColorRGBA.Magenta;
		for (int lineIdx=0; lineIdx < numLines; lineIdx++) {
			Line aLine = new Line (baseX, baseY, width, height);
			parentNode.attachChild(aLine.getRenderingSpatial());
			myLines.add(aLine);
		}
	}
	public void displayScore(int lineNum, String scoreText) {
		Line l = myLines.get(lineNum);
		l.setScoreText(scoreText);
	}
}
