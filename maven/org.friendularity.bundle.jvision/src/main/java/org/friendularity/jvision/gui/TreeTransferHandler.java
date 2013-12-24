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

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.TransferHandler;
import javax.swing.tree.TreePath;
import org.friendularity.jvision.filters.ClassFilterInfo;

/**
 *
 * @author Annie
 */
class TreeTransferHandler extends TransferHandler {
    static DataFlavor localObjectFlavor;
	
    static {
        try {
            localObjectFlavor =
                new DataFlavor (DataFlavor.javaJVMLocalObjectMimeType);
        } catch (ClassNotFoundException cnfe) { cnfe.printStackTrace(); }
    }
    static DataFlavor[] supportedFlavors = { localObjectFlavor };
	
	JTree mytree;
	
	public TreeTransferHandler(JTree atree) {
		mytree = atree;
	}

	@Override
	protected Transferable createTransferable(final JComponent c) {
		return new Transferable() {

			@Override
			public DataFlavor[] getTransferDataFlavors() {
				return supportedFlavors;
			}

			@Override
			public boolean isDataFlavorSupported(DataFlavor flavor) {
				return flavor.equals(localObjectFlavor);
			}

			@Override
			public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
				if(!flavor.equals(localObjectFlavor))
					throw new UnsupportedFlavorException(flavor);
				
				TreePath tp = ((FilterTree)c).getSelectionPath();
				
				Object o = tp.getLastPathComponent();
				if(!(o instanceof ClassFilterInfo))
					throw new java.lang.IllegalArgumentException("hey that wasn't a ClassFilterInfo");
				
				return o;
			}
		};
	}
	
}
