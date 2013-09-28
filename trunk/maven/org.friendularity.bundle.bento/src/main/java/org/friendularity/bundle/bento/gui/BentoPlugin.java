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
package org.friendularity.bundle.bento.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import org.friendularity.bundle.bento.util.Bento_OSGi_ResourceLoader;

/**
 *
 * @author Annie
 */
class BentoPlugin  extends JPanel implements ActionListener {
	public BentoPlugin() {
		try {
			initPopup();
		} catch (IOException ex) {
			Logger.getLogger(BentoPlugin.class.getName()).log(Level.SEVERE, null, "missing resources probably wont come up");
		}
	}
	
	protected void handleActions(ActionEvent e)
	{
		System.err.println(e.getActionCommand());
	}
	
	/* ================= Popup menu handling ================ */
	
	/**
	 * Add additional menu items at startup'
	 * Safe to do this on any thread
	 * 
	 * @param menu 
	 */
	protected void addAdditionalMenuItems(JPopupMenu menu)
	{
		
	}
	
	/**
	 * Override only to completely replace popup menu
	 * 
	 * you probably want addAdditionalMenuItems
	 * 
	 * 
	 */
	protected void initPopup() throws IOException
	{
		
		ImageIcon ii = new ImageIcon(
				Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/twohor.png"));
		
		popup = new JPopupMenu();
		JMenuItem menuItem = new JMenuItem("Split Horizontal", ii);
		menuItem.addActionListener(this);
		popup.add(menuItem);
		
		ii = new ImageIcon(
				Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/threehor.png"));
		menuItem = new JMenuItem("Split 3 Horizontal", ii);
		menuItem.addActionListener(this);
		popup.add(menuItem);
		
		addAdditionalMenuItems(popup);

		//Add listener to components that can bring up popup menus.
		MouseListener popupListener = new PopupListener();
		this.addMouseListener(popupListener);
		
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		handleActions(e);
	}
	
   private JPopupMenu popup;
   
   /**
	* get the right click menu
	* 
	* If you modify it, do it on swingworker thread
	* 
	* @return popup menu
	*/
   protected JPopupMenu getPopup()
   {
	   return popup;
   }
	
	class PopupListener extends MouseAdapter {
		public void mousePressed(MouseEvent e) {
			maybeShowPopup(e);
		}

		public void mouseReleased(MouseEvent e) {
			maybeShowPopup(e);
		}

		private void maybeShowPopup(MouseEvent e) {
			if (e.isPopupTrigger()) {
				popup.show(e.getComponent(),
						   e.getX(), e.getY());
			}
		}
	}
}
