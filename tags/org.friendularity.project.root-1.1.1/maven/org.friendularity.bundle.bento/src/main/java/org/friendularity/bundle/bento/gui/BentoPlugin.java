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

import org.friendularity.bundle.bento.util.Bento_OSGi_ResourceLoader;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;

import javax.swing.*;

/**
 * @author Annie
 */
public abstract class BentoPlugin extends JPanel implements ActionListener {

	private BentoBox myBentoController;

	public BentoPlugin() {
		this.setLayout(null);
		this.addMouseMotionListener(new MouseAdapter() {

			@Override
			public void mouseMoved(MouseEvent e) {
				super.mouseMoved(e);
				BentoPlugin.this.setMoveCursor();
			}
		});
	}


	@Override
	public boolean isOptimizedDrawingEnabled() {
		return false; // ensures that z order happens properly
	}

	protected void handleActions(ActionEvent e) {
		System.err.println(e.getActionCommand());
		containingBox().doCommand(this, e.getActionCommand());
	}

	/**
	 * call after adding to hierarchy
	 */

	void init(BentoBox aController) {
		myBentoController = aController;

		try {
			initPopup();
		} catch (IOException ex) {
			LoggerFactory.getLogger(BentoPlugin.class.getName()).error("missing resources probably wont come up");
		}
	}
	/* ================= Popup menu handling ================ */

	/**
	 * Add additional menu items at startup
	 * Safe to do this on any thread
	 */
	protected void addAdditionalMenuItems(JPopupMenu menu) {

	}

	/**
	 * Return the bentobox we're within, if we're within one
	 * or null
	 */
	protected BentoBox containingBox() {
		Container x = this;

		while (x != null) {
			if (x instanceof BentoBox)
				return (BentoBox) x;

			x = x.getParent();
		}

		return null;
	}

	/**
	 * true iff we are contained in a BentoBox and can split/merge
	 */
	boolean canSplit() {
		return containingBox() != null;
	}

	// @TODO move menu to BentoAction
	public static final String HTWO_MENU = "H Two";
	public static final String HTHREE_MENU = "H Three";
	public static final String HFOUR_MENU = "H Four";
	public static final String VTWO_MENU = "V Two";
	public static final String VTHREE_MENU = "V Three";
	public static final String VFOUR_MENU = "V Four";
	public static final String REMOVE_MENU = "Remove";

	/**
	 * Override only to completely replace popup menu
	 *
	 * you probably want addAdditionalMenuItems
	 */
	protected void initPopup() throws IOException {
		popup = new JPopupMenu();

		if (canSplit()) {
			JMenu subMenu = new JMenu("Hor. Split");
			ImageIcon ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/twohor.png"));
			JMenuItem menuItem = new JMenuItem(HTWO_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/threehor.png"));
			menuItem = new JMenuItem(HTHREE_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/fourhor.png"));
			menuItem = new JMenuItem(HFOUR_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			popup.add(subMenu);

			subMenu = new JMenu("Vert. Split");
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/twovert.png"));
			menuItem = new JMenuItem(VTWO_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/threevert.png"));
			menuItem = new JMenuItem(VTHREE_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			ii = new ImageIcon(
					Bento_OSGi_ResourceLoader.getDefaultImageLoader().getImageResource("/img/fourvert.png"));
			menuItem = new JMenuItem(VFOUR_MENU, ii);
			menuItem.addActionListener(this);
			subMenu.add(menuItem);
			popup.add(subMenu);

			menuItem = new JMenuItem(REMOVE_MENU);
			menuItem.addActionListener(this);
			popup.add(menuItem);
		}

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
	protected JPopupMenu getPopup() {
		return popup;
	}

	public BentoBox getBentoBox() {
		return myBentoController;
	}

	private void setMoveCursor() {
		try {
			((MergeGrid) this.getParent()).getGlassPane().setCursor(Cursor.getDefaultCursor());
		} catch (Exception e) {
			System.err.println("vastly painful");
		}

	}

	void removeFromMergeGrid() {
		((MergeGrid) this.getParent()).removeCell(this);
		pluginRemoved();
	}

	protected void pluginRemoved() {

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
