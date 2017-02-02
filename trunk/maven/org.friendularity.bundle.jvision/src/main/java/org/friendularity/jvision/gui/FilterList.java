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

import org.friendularity.jvision.filters.BaseFilter;
import org.friendularity.jvision.filters.FilterSequence;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.IOException;

import javax.swing.*;

/**
 * @author Annie
 */
public class FilterList extends JList
		implements DragSourceListener, DropTargetListener, DragGestureListener {

	static DataFlavor localObjectFlavor;

	static {
		try {
			localObjectFlavor =
					new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
		} catch (ClassNotFoundException cnfe) {
			cnfe.printStackTrace();
		}
	}

	static DataFlavor[] supportedFlavors = {localObjectFlavor};
	DragSource dragSource;
	DropTarget dropTarget;
	Object dropTargetCell;
	int draggedIndex = -1;

	public FilterList() {
		super();
		setCellRenderer(new ReorderableListCellRenderer());
		setModel(new DefaultListModel());
		dragSource = new DragSource();
		DragGestureRecognizer dgr =
				dragSource.createDefaultDragGestureRecognizer(this,
						DnDConstants.ACTION_MOVE,
						this);
		dropTarget = new DropTarget(this, this);
	}

	// DragGestureListener
	public void dragGestureRecognized(DragGestureEvent dge) {
		System.out.println("dragGestureRecognized");
		// find object at this x,y
		Point clickPoint = dge.getDragOrigin();
		int index = locationToIndex(clickPoint);
		if (index == -1)
			return;
		Object target = getModel().getElementAt(index);
		Transferable trans = new RJLTransferable(target);
		draggedIndex = index;
		dragSource.startDrag(dge, Cursor.getDefaultCursor(),
				trans, this);
	}

	// DragSourceListener events
	public void dragDropEnd(DragSourceDropEvent dsde) {
		System.out.println("dragDropEnd()");
		dropTargetCell = null;
		draggedIndex = -1;
		repaint();
	}

	@Override
	public void dragEnter(DragSourceDragEvent dsde) {
	}

	@Override
	public void dragExit(DragSourceEvent dse) {
	}

	@Override
	public void dragOver(DragSourceDragEvent dsde) {
	}

	@Override
	public void dropActionChanged(DragSourceDragEvent dsde) {
	}

	// DropTargetListener events
	@Override
	public void dragEnter(DropTargetDragEvent dtde) {
		System.out.println("dragEnter");
		if (dtde.getSource() != dropTarget) {
			System.out.println("not self, rejecting dragEnter");
			dtde.rejectDrag();
		} else {
			dtde.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE);
			System.out.println("accepted dragEnter");
		}

	}

	public void dragExit(DropTargetEvent dte) {
	}

	public void dragOver(DropTargetDragEvent dtde) {
		// figure out which cell it's over, no drag to self
		if (dtde.getSource() != dropTarget) {
			System.out.println("not self, rejecting dragOver");
			dtde.rejectDrag();
			return;
		}
		Point dragPoint = dtde.getLocation();
		int index = locationToIndex(dragPoint);
		if (index == -1)
			dropTargetCell = null;
		else
			dropTargetCell = getModel().getElementAt(index);
		repaint();
	}

	public void drop(DropTargetDropEvent dtde) {
		System.out.println("drop()!");
		if (!dtde.isDataFlavorSupported(localObjectFlavor)) {
			System.out.println("rejecting because cannot get localObjectFlavor (" +
					dtde.getSource().getClass().getName() + ")");
			dtde.rejectDrop();
			return;
		}

		Point dropPoint = dtde.getLocation();
		int index = locationToIndex(dropPoint);
		System.out.println("drop index is " + index);

		if (dtde.getSource() != dropTarget) {
			System.out.println("foreign drop from (" +
					dtde.getSource().getClass().getName() + ")");
			dtde.acceptDrop(DnDConstants.ACTION_COPY);
			FilterSequence mod = (FilterSequence) getModel();
			try {
				mod.addOrReplaceByClass((BaseFilter) dtde.getTransferable().getTransferData(localObjectFlavor));
			} catch (UnsupportedFlavorException ex) {
				LoggerFactory.getLogger(FilterList.class).error(ex.getMessage(), ex);
			} catch (IOException ex) {
				LoggerFactory.getLogger(FilterList.class).error(ex.getMessage(), ex);
			}
			return;
		}

		boolean dropped = false;
		try {
			if (index == -1) {
				System.out.println("dropped onto -1");
				dtde.rejectDrop();
				return;
			}
			if (index == draggedIndex) {
				System.out.println("dropped onto self");
				dtde.rejectDrop();
				return;
			}

			dtde.acceptDrop(DnDConstants.ACTION_MOVE);
			System.out.println("accepted");
			Object dragged =
					dtde.getTransferable().getTransferData(localObjectFlavor);
			// move items - note that indicies for insert will
			// change if [removed] source was before target
			System.out.println("drop " + draggedIndex + " to " + index);
			boolean sourceBeforeTarget = (draggedIndex < index);
			System.out.println("source is" +
					(sourceBeforeTarget ? "" : " not") +
					" before target");
			System.out.println("insert at " +
					(sourceBeforeTarget ? index - 1 : index));
			FilterSequence mod = (FilterSequence) getModel();
			mod.move(draggedIndex, (sourceBeforeTarget ? index - 1 : index));
			///     mod.remove (draggedIndex);
			//     mod.add ((sourceBeforeTarget ? index-1 : index), dragged);
			dropped = true;
		} catch (Exception e) {
			e.printStackTrace();
		}
		dtde.dropComplete(dropped);
	}

	public void dropActionChanged(DropTargetDragEvent dtde) {
	}

	// test
	public static void showReorderableFilterList(ListModel defModel) {
		JList list = new FilterList();

		list.setModel(defModel);

		// show list
		JScrollPane scroller =
				new JScrollPane(list,
						ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
						ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		JFrame frame = new JFrame("Drag to reorder filters");
		frame.getContentPane().add(scroller);
		frame.pack();
		frame.setVisible(true);
	}

	class RJLTransferable implements Transferable {
		Object object;

		public RJLTransferable(Object o) {
			object = o;
		}

		public Object getTransferData(DataFlavor df)
				throws UnsupportedFlavorException, IOException {
			if (isDataFlavorSupported(df))
				return object;
			else
				throw new UnsupportedFlavorException(df);
		}

		public boolean isDataFlavorSupported(DataFlavor df) {
			return (df.equals(localObjectFlavor));
		}

		public DataFlavor[] getTransferDataFlavors() {
			return supportedFlavors;
		}
	}

	// custom renderer
	class ReorderableListCellRenderer
			extends DefaultListCellRenderer {
		boolean isTargetCell;
		boolean isLastItem;
		Insets normalInsets, lastItemInsets;
		int BOTTOM_PAD = 30;

		public ReorderableListCellRenderer() {
			super();
			normalInsets = super.getInsets();
			lastItemInsets =
					new Insets(normalInsets.top,
							normalInsets.left,
							normalInsets.bottom + BOTTOM_PAD,
							normalInsets.right);
		}

		public Component getListCellRendererComponent(JList list,
													  Object value,
													  int index,
													  boolean isSelected,
													  boolean hasFocus) {
			isTargetCell = (value == dropTargetCell);
			isLastItem = (index == list.getModel().getSize() - 1);
			boolean showSelected = isSelected &
					(dropTargetCell == null);
			return super.getListCellRendererComponent(list, value,
					index, showSelected,
					hasFocus);
		}

		/*
		// this isn't helping, neither does overriding getSize()
		public Insets getInsets(Insets in) {
			Insets useInsets = (isLastItem ? lastItemInsets : normalInsets);
			in.top = useInsets.top;
			in.bottom = useInsets.bottom;
			in.left = useInsets.left;
			in.right = useInsets.right;
			return in;
		}
		*/
		public void paintComponent(Graphics g) {
			super.paintComponent(g);
			if (isTargetCell) {
				g.setColor(Color.black);
				g.drawLine(0, 0, getSize().width, 0);
			}
		}
	}
}

/*   Swing Hacks
 *   Tips and Tools for Killer GUIs
 * By Joshua Marinacci, Chris Adamson
 *   First Edition June 2005
 *   Series: Hacks
 *   ISBN: 0-596-00907-0
 *   http://www.oreilly.com/catalog/swinghks/
 */