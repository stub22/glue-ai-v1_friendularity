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
package org.friendularity.jvision.filters;

import java.awt.Container;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 *
 * @author Annie
 */
class FilterParams extends JFrame {
    private ParamChangedListener pcl = null;
	
	/**
	 * 
	 * @param aFilter
	 * @param builderStr 
	 */
	static void showParamWidget(ParamChangedListener aFilter, String builderStr) {
		new FilterParams(aFilter, builderStr);
	}

	private FilterParams(ParamChangedListener aFilter, String builderStr) {
		pcl = aFilter;
		Container c = this.getContentPane();
		c.setLayout(new BoxLayout(c, BoxLayout.Y_AXIS));
		String[] s = builderStr.split(",");
		
		int index = 0;
		for(int i = 0 ; i < s.length ; ) {
			c.add(new JLabel(s[i]));
			
			if(s[i+1].equals("slider")) {
				JSlider sldr = new JSlider(JSlider.HORIZONTAL);
				sldr.setMinimum((int)Double.parseDouble(s[i+2]));
				sldr.setMaximum((int)Double.parseDouble(s[i+4]));
				sldr.setValue((int)Double.parseDouble(s[i+3]));
				sldr.addChangeListener(new indexChangeListener(index, pcl));
				c.add(sldr);
				i += 5;
			} else {
				i++;
			}
			index++;
			
		}
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		this.setVisible(true);
		c.invalidate();
		this.pack();
	}

	private static class indexChangeListener implements ChangeListener {
        private int index;
		private ParamChangedListener pcl;
		
		public indexChangeListener(int index, ParamChangedListener pcl) {
			this.index = index;
			this.pcl = pcl;
		}

		@Override
		public void stateChanged(ChangeEvent e) {
			if( e.getSource() instanceof JSlider) {
				pcl.paramChanged(index, Integer.toString(((JSlider)(e.getSource())).getValue()));
			} else {
				pcl.paramChanged(index, e.getSource().toString());
			}
		}
	}
	
}
