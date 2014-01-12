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

import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import org.friendularity.jvision.filters.FilterInfo;
import org.friendularity.jvision.filters.FilterInfoManager;

/**
 *
 * @author Annie
 */
public class FilterTree extends JTree implements TreeSelectionListener {

	public static final int ALL_FILTERS = 1;
	
    //Optionally play with line styles.  Possible values are
    //"Angled" (the default), "Horizontal", and "None".
    private static boolean playWithLineStyle = false;
    private static String lineStyle = "Angled";
    private static boolean DEBUG = true;
	private FilterInfo currentSelection = null;
	
	static DefaultMutableTreeNode nodeFactory(int selection_rule) {
		if(selection_rule != ALL_FILTERS)
			throw new UnsupportedOperationException("Filter filtering not implemented yet");
		
        //Create the nodes.
        DefaultMutableTreeNode top =
            new DefaultMutableTreeNode("Flo Filters");

		FilterInfoManager.addAllFilterInfoToTree(top);
		
		return top;
    }
        
	FilterTree(DefaultMutableTreeNode top) {
		super(top);
		
		getSelectionModel().setSelectionMode
                (TreeSelectionModel.SINGLE_TREE_SELECTION);

        //Listen for when the selection changes.
        addTreeSelectionListener(this);
		
//		setDragEnabled(true);
		
	//	setDropMode(DropMode.INSERT);
	//	setTransferHandler(new TreeTransferHandler(this));

        if (playWithLineStyle) {
            System.out.println("line style = " + lineStyle);
            putClientProperty("JTree.lineStyle", lineStyle);
        }
	}

	
	FilterInfo getCurrentFilterSelectionOrNull() {
		return currentSelection;
	}
	
	// ======================== TreeSelectionListener interface ====================

    /** Required by TreeSelectionListener interface. */
    public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)getLastSelectedPathComponent();

        if (node == null) return;

        Object nodeInfo = node.getUserObject();
        if (node.isLeaf()) {
            currentSelection = (FilterInfo)nodeInfo;
        } else {
           currentSelection = null;
        }
    }
}
