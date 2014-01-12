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

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.NodeFactory;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonModel;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.friendularity.jvision.engine.CVChain;
import org.friendularity.jvision.engine.CVChainManager;
import org.friendularity.jvision.engine.JVisionRDF;
import org.friendularity.jvision.filters.BaseFilter;
import org.friendularity.jvision.filters.FilterSequence;

/**
 *  The GUI panel associated with a CVChain
 * 
 * @author Annie
 */
public class CVChainControl extends JPanel {
	private CVChain chain;
	
	private JLabel nameField;
	private JCheckBox publishIntermediatesCheck;
	private FilterList filters;
	private JLabel sourceField;
	private JLabel outField;
	private FilterBox myFilterBox;

	@Deprecated
	CVChainControl(FilterBox fb, Model M, Resource cvchain) {		
		CVChain chain = new CVChain(M, cvchain);
		
		init(chain, fb);
	}
	
	private Border borderFactory() {
		return BorderFactory.createCompoundBorder(
				BorderFactory.createEmptyBorder(6, 6, 6, 6),  // margin tlbr
				BorderFactory.createCompoundBorder(
					BorderFactory.createEtchedBorder(EtchedBorder.RAISED, 
						getBackground().brighter(), 
						getBackground().darker()),
					BorderFactory.createEmptyBorder(6, 6, 6, 6)));   // padding tlbr
	}
	
	public CVChainControl(CVChain chain, FilterBox aFilterBox) {
		init(chain, aFilterBox);
	}
	
	private void init(CVChain chain, FilterBox aFilterBox) {
		this.chain = chain;
		this.myFilterBox = aFilterBox;
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		
		JPanel nameBox = new JPanel();
		nameBox.setLayout(new BoxLayout(nameBox, BoxLayout.Y_AXIS));
		nameBox.setBorder(borderFactory());
		nameBox.add(new JLabel("name:"));
		nameField = new JLabel(chain.getName());
		nameBox.add(nameField);
		add(nameBox);
		nameBox.setAlignmentX(CENTER_ALIGNMENT);
		
		
		JPanel sourceBox = new JPanel();
		sourceBox.setLayout(new BoxLayout(sourceBox, BoxLayout.Y_AXIS));
		sourceBox.setAlignmentX(CENTER_ALIGNMENT);
		JLabel sourceFieldLabel = new JLabel("Source:");
		sourceBox.add(sourceFieldLabel);
		
		sourceField = new JLabel(chain.getSource());
		sourceBox.add(sourceField);
		sourceBox.setBorder(borderFactory());
		sourceBox.setAlignmentX(CENTER_ALIGNMENT);
		
		add(sourceBox);
		
		publishIntermediatesCheck = new JCheckBox("publish intermediates");
		
		publishIntermediatesCheck.addChangeListener(new ChangeListener(){
			@Override
			public void stateChanged(ChangeEvent e) {
				CVChainControl.this.chain.setPublishIntermediates(publishIntermediatesCheck.getModel().isSelected());
			}
		});
		publishIntermediatesCheck.setAlignmentX(CENTER_ALIGNMENT);
		
		add(publishIntermediatesCheck);
		
		JPanel filterListBox = new JPanel();
		filterListBox.setLayout(new BoxLayout(filterListBox, BoxLayout.Y_AXIS));
		JLabel flbLabel = new JLabel("filters");
		flbLabel.setAlignmentX(CENTER_ALIGNMENT);
		filterListBox.add(flbLabel);
		
		filters = new FilterList();
		filters.setModel(chain.getFilterSequence());
		filterListBox.setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createEmptyBorder(6, 6, 6, 6), 
				BorderFactory.createLineBorder(Color.BLACK)));
		filterListBox.add(filters);
		add(filterListBox);
		
		outField = new JLabel(chain.getOutName());
		outField.setAlignmentX(CENTER_ALIGNMENT);
		outField.setBorder(borderFactory());
		add(outField);
		this.setMinimumSize(new Dimension(0, 600));
		this.setAlignmentY(TOP_ALIGNMENT);
		this.setAlignmentX(CENTER_ALIGNMENT);
		
		this.setBorder(new CVChainControlBorder(this));
		
		this.addMouseListener(new MouseAdapter(){

			CVChainControl oldSelection = null;
			
			@Override
			public void mousePressed(MouseEvent e) {
				super.mousePressed(e); 
				oldSelection = myFilterBox.getSelectedCVChainControl();
				myFilterBox.setSelectedChainControl(CVChainControl.this);
			}

			@Override
			public void mouseReleased(MouseEvent e) {
				super.mouseReleased(e); 
				if(e.getX() < 0 || e.getY() < 0 ||
				   e.getX() >= CVChainControl.this.getWidth() ||
				   e.getY() >= CVChainControl.this.getHeight()) {
					myFilterBox.setSelectedChainControl(oldSelection);
				}
			}
			
		});
	}

	@Override
	public String getName() {
		return chain.getName();
	}

	boolean isSelected() {
		CVChainControl cvcc = myFilterBox.getSelectedCVChainControl();
		// cvcc might be null
		return (this == cvcc);
	}

	FilterSequence getFilterSequence() {
		return (FilterSequence)(filters.getModel());
	}

	void removeSelectedFilter() {
		int i = filters.getSelectedIndex();
		if(i >= 0) {
			((FilterSequence)(filters.getModel())).remove(i);
		}
	}

	void addSelfToModel(Model M) {
		Resource cv = M.createResource(JVisionRDF.CV_PREFIX + "cvchain" + chain.getName());
		M.add(M.createStatement(cv, M.createProperty(JVisionRDF.RDF_PREFIX + "type") , 
				M.createResource(JVisionRDF.FLO_PREFIX + "CVChain")));
		M.add(M.createStatement(cv, M.createProperty(JVisionRDF.RDF_PREFIX + "label"), M.createLiteral(chain.getName())));
		M.add(M.createStatement(cv, M.createProperty(JVisionRDF.FLO_PREFIX + "source"), M.createLiteral(chain.getSource())));
		M.add(M.createStatement(cv, M.createProperty(JVisionRDF.FLO_PREFIX + "intermediatesVisible"), 
				M.createLiteral(Boolean.FALSE.toString())));
		
		FilterSequence fs = (FilterSequence)(chain.getFilterSequence());
		
		for(int i = 0 ; i < fs.getSize() ; i++) {
			String f = fs.getElementAt(i).toString();
			Resource frsrc = M.createResource(JVisionRDF.CV_PREFIX + "filter/" + chain.getName() + "/" + Integer.toString(i));
			M.add(M.createStatement(frsrc, 
					M.createProperty(JVisionRDF.RDF_PREFIX + "type"), 
					M.createResource(JVisionRDF.FLO_PREFIX + "FilterInstance")));
			M.add(M.createStatement(frsrc, M.createProperty(JVisionRDF.FLO_PREFIX + "inChain"), cv)); // filterType  index inChain
			M.add(M.createStatement(frsrc, M.createProperty(JVisionRDF.FLO_PREFIX + "index"), 
					M.createLiteral(Integer.toString(i))));
			M.add(M.createStatement(frsrc, M.createProperty(JVisionRDF.FLO_PREFIX + "filterType"), 
					M.createLiteral(f)));
			M.add(M.createStatement(frsrc, M.createProperty(JVisionRDF.FLO_PREFIX + "filterParameters"), 
					M.createLiteral(fs.serializeIndex(i))));
		}
	}

	void showParametersOfSelectedFilter() {
		int i = filters.getSelectedIndex();
		
		if(i >= 0) {
			 ((FilterSequence)(filters.getModel())).showParamUIForIndex(i);
		}
	}
}
