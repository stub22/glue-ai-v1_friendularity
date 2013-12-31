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

import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import org.friendularity.jvision.filters.BananaDetector;
import org.friendularity.jvision.filters.Blur;
import org.friendularity.jvision.filters.StatelessClassFilterInfo;
import org.friendularity.jvision.filters.ColorThreshold;
import org.friendularity.jvision.filters.Contour;
import org.friendularity.jvision.filters.Dilate;
import org.friendularity.jvision.filters.Erode;
import org.friendularity.jvision.filters.FaceDetector;
import org.friendularity.jvision.filters.Farneback;
import org.friendularity.jvision.filters.FilterInfo;
import org.friendularity.jvision.filters.GlassesDetector;
import org.friendularity.jvision.filters.Grayscale;
import org.friendularity.jvision.filters.ProfileDetector;
import org.friendularity.jvision.filters.RGBtoHSV;

/**
 *
 * @author Annie
 */
public class FilterTree extends JTree implements TreeSelectionListener, 
		DragSourceListener, DropTargetListener, DragGestureListener {

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

        DefaultMutableTreeNode category = null;
        DefaultMutableTreeNode filter = null;

        category = new DefaultMutableTreeNode("Detector");
        top.add(category);

        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new BananaDetector()));
        category.add(filter);

        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new FaceDetector()));
        category.add(filter);
		
        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new GlassesDetector()));
        category.add(filter);

        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new ProfileDetector()));
        category.add(filter);
		
        category = new DefaultMutableTreeNode("Image Processing");
        top.add(category);
		
        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new Blur()));
        category.add(filter);

        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new ColorThreshold()));
        category.add(filter);

        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new Grayscale()));
        category.add(filter);

        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new RGBtoHSV()));
        category.add(filter);
		
        category = new DefaultMutableTreeNode("Line Operations");
        top.add(category);
		
		filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new Contour()));
        category.add(filter);
		
        category = new DefaultMutableTreeNode("Morphology");
        top.add(category);
				
        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new Dilate()));
        category.add(filter);		
		
        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new Erode()));
        category.add(filter);		

        category = new DefaultMutableTreeNode("Motion");
        top.add(category);
		
        filter = new DefaultMutableTreeNode(new StatelessClassFilterInfo (new Farneback()));
        category.add(filter);
		
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
	
	
	FilterInfo getCurrentFilterSelectionOrNull() {
		return currentSelection;
	}

	// ================ DragSourceListener interface =======================
	@Override
    public void dragEnter (DragSourceDragEvent dsde) {}
	@Override
    public void dragExit (DragSourceEvent dse) {}
	@Override
    public void dragOver (DragSourceDragEvent dsde) {}
	@Override
    public void dropActionChanged (DragSourceDragEvent dsde) {}

	@Override
	public void dragDropEnd(DragSourceDropEvent dsde) {
		// a drag has ended. Don't believe we have to do anything with it
	}

	// ================= Implements DropTargetListener =====================
	
	@Override
	public void dragEnter(DropTargetDragEvent dtde) {
		dtde.rejectDrag();
	}

	@Override
	public void dragOver(DropTargetDragEvent dtde) {
		dtde.rejectDrag();
	}

	@Override
	public void dropActionChanged(DropTargetDragEvent dtde) {
		dtde.rejectDrag();
	}

	@Override
	public void dragExit(DropTargetEvent dte) { }

	@Override
	public void drop(DropTargetDropEvent dtde) {
		dtde.rejectDrop();
	}

	// ====================== implements DragGestureListener ============================
	
	@Override
	public void dragGestureRecognized(DragGestureEvent dge) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

}
