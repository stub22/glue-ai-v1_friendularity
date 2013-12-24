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

import org.friendularity.jvision.engine.CVChainManager;
import org.friendularity.jvision.engine.CVChain;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import org.friendularity.jvision.broker.ImageStreamBroker;

/**
 *
 * @author Annie
 */
class CVChainInfoDialog extends JDialog implements DocumentListener, ChangeListener, ListSelectionListener {

	private JTextField nameField;
	private JCheckBox intermediatesVisible;
	private JList sources;
	private JButton ok;
	private FilterBox fb;
	private CVChainManager cvccm;
	
	public CVChainInfoDialog(FilterBox fb, CVChainManager cvccm) {

		super(fb.getFrame(), "CVChainInfo", true);
		this.fb = fb;
		this.cvccm = cvccm;
		
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		JComponent cp = (JComponent) this.getContentPane();
		
		cp.setLayout(new BoxLayout(cp, BoxLayout.Y_AXIS));
		cp.add(new JLabel("ImageStream name prefix"));
		
		nameField = new JTextField("name here");
		cp.add(nameField);
		nameField.setMaximumSize(new Dimension(32767, 24));
		nameField.getDocument().addDocumentListener(this);
		
		intermediatesVisible = new JCheckBox("publish intermediate steps");
		cp.add(intermediatesVisible);
		intermediatesVisible.addChangeListener(this);
		
		cp.add(new JLabel("Feed CVChain from this source"));
		
		sources = new JList(){
			@Override
			protected void paintBorder(Graphics g) {
				g.setColor(Color.black);
				g.drawRect(0, 0, this.getWidth() - 1, this.getHeight() - 1);
			}
		};
		
		DefaultListModel lm = new DefaultListModel();
		sources.setModel(lm);

		cp.add(sources);
		
		int item_index = 0;
		for(Iterator<String>names = 
				ImageStreamBroker.getDefaultImageStreamBroker().imageStreamNames();
				names.hasNext() ; ) {
			lm.add(item_index++, names.next());
		}
		
		sources.addListSelectionListener(this);
		JPanel pnl = new JPanel();
		pnl.setLayout(new BoxLayout(pnl, BoxLayout.X_AXIS));
		ok = new JButton("OK");
		ok.setDefaultCapable(true);
		ok.setEnabled(false);
		ok.addActionListener(new ActionListener(){ 

			@Override
			public void actionPerformed(ActionEvent e) {
				CVChainInfoDialog.this.doOK();
			}
		});
		
		JButton cancel = new JButton("Cancel");
		cancel.setDefaultCapable(false);
		
		cancel.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				CVChainInfoDialog.this.dispose();
			}
		});
		
		pnl.add(ok);
		pnl.add(cancel);
		cp.add(pnl);
	
		this.setSize(new Dimension(400, 400));
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		updateOKToOK();
	}

	@Override
	public void insertUpdate(DocumentEvent e) {
		updateOKToOK();
	}

	@Override
	public void removeUpdate(DocumentEvent e) {
		updateOKToOK();
	}

	@Override
	public void changedUpdate(DocumentEvent e) {
		updateOKToOK();
	}

	@Override
	public void valueChanged(ListSelectionEvent e) {
		updateOKToOK();
	}

	private void updateOKToOK() {
		String name = nameField.getText().trim();
		
		ok.setEnabled(name.length() > 0 &&
				!name.equals("name here") &&
				sources.getSelectedIndex() != -1 );
		
		if( !ImageStreamBroker.getDefaultImageStreamBroker().imageStreamPrefixOK(name) )
			ok.setEnabled(false);
		if( CVChainManager.getDefaultManager().chainExists(name))
			ok.setEnabled(false);
	}

	private void doOK() {
		System.out.println("OK fired");
		cvccm.buildChain(fb, 
				nameField.getText().trim(), 
				intermediatesVisible.getModel().isSelected() , 
				(String)(sources.getModel().getElementAt(sources.getSelectedIndex()))
				);
		
		
		dispose();
	}
	
}
