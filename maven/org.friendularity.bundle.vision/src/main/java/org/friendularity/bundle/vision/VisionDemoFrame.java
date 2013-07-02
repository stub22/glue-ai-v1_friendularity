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

package org.friendularity.bundle.vision;

import java.awt.BorderLayout;
import javax.swing.JFrame;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class VisionDemoFrame extends JFrame {
    private org.rwshop.swing.vision.VideoControlPanel	myVideoControlPanel;
    private org.rwshop.swing.vision.VideoPanel			myVideoPanel;
	
	public void setup() { 
		
        myVideoPanel = new org.rwshop.swing.vision.VideoPanel();
        myVideoControlPanel = new org.rwshop.swing.vision.VideoControlPanel();
		
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(myVideoControlPanel, BorderLayout.NORTH);
		getContentPane().add(myVideoPanel, BorderLayout.CENTER);
		// Does:
		// 1)   videoService.addImageListener(myVideoPanel.getImageEventListener());
        // 2)	faceService.addImageRegionsListener(myVideoPanel.getImageRegionListListener());
		myVideoControlPanel.setVideoPanel(myVideoPanel);
	}
   	
}
