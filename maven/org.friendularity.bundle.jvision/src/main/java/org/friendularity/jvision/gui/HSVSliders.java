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
package org.friendularity.jvision.gui;

import java.awt.Container;
import java.awt.HeadlessException;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.friendularity.jvision.filters.ColorThreshold;

/**
 *
 * @author Annie
 */
public class HSVSliders extends JFrame {

	public HSVSliders() throws HeadlessException {
		this.setSize(180, 500);
		
		Container c = this.getContentPane();
		c.setLayout(new java.awt.FlowLayout());
		c.add(new JLabel("Color Threshold"));
		
		addSlider("min H or B", c, 50, ColorThreshold.MIN_H);
		addSlider("min S or G", c, 50, ColorThreshold.MIN_S);
		addSlider("min V or R", c, 50, ColorThreshold.MIN_V);
		addSlider("max H or B", c, 205, ColorThreshold.MAX_H);
		addSlider("max S or G", c, 205, ColorThreshold.MAX_S);
		addSlider("max V or R", c, 205, ColorThreshold.MAX_V);
	}

	private void addSlider(String label, Container c, int initial, final int which) {
		
		c.add(new JLabel(label));
		JSlider s = new JSlider();
		
		s.setMinimum(0);
		s.setMaximum(255);
		s.setValue(initial);
		s.addChangeListener(
				new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				ColorThreshold.changeLimit(
						((JSlider) (e.getSource())).getValue(),
						which);
			}
		});
		
		c.add(s);
	}
	
}
