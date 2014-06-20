package org.friendularity.gui.freckle;


import java.awt.Image;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

class FaceTransferHandler extends TransferHandler {
    DataFlavor pictureFlavor = DataFlavor.imageFlavor;
    Image sourcePic;
    boolean shouldRemove;

    public boolean importData(JComponent c, Transferable t) {
        Image image;
        if (canImport(c, t.getTransferDataFlavors())) {
        	try {
        		PersonPanel pic = (PersonPanel)c;

        		try {
        			image = (Image)t.getTransferData(pictureFlavor);
        			//Set the component to the new picture.
        			pic.addImage(image);
        			return true;
        		} catch (UnsupportedFlavorException ufe) {
        			System.out.println("importData: unsupported data flavor");
        		} catch (IOException ioe) {
        			System.out.println("importData: I/O exception");
        		}
            } catch (ClassCastException e) {
            	// fall through
            }
            try {
            	ServerControlPanel s = (ServerControlPanel)c;
            	
            	try {
            		s.setQueryImage(ServerVizUI.toBufferedImage((Image)t.getTransferData(pictureFlavor), true));
        		} catch (IOException ioe) {
        			System.out.println("importData: I/O exception");
            	} catch (UnsupportedFlavorException ufe) {
            		System.out.println("importData: unsupported data flavor");
            	}
            } catch (ClassCastException e) {
            	// fall through
            }
        }
        return false;
    }

    protected Transferable createTransferable(JComponent c) {
        sourcePic = ((ImagePanel)c).dragImage();
        return new PictureTransferable(sourcePic);
    }

    public int getSourceActions(JComponent c) {
        return COPY_OR_MOVE;
    }

    protected void exportDone(JComponent c, Transferable data, int action) {
        sourcePic = null;
    }

    public boolean canImport(JComponent c, DataFlavor[] flavors) {
        for (int i = 0; i < flavors.length; i++) {
            if (pictureFlavor.equals(flavors[i])) {
                return true;
            }
        }
        return false;
    }

    class PictureTransferable implements Transferable {
        private Image image;

        PictureTransferable(Image pic) {
            image = pic;
        }

        public Object getTransferData(DataFlavor flavor)
                                 throws UnsupportedFlavorException {
            if (!isDataFlavorSupported(flavor)) {
                throw new UnsupportedFlavorException(flavor);
            }
            return image;
        }

        public DataFlavor[] getTransferDataFlavors() {
            return new DataFlavor[] { pictureFlavor };
        }

        public boolean isDataFlavorSupported(DataFlavor flavor) {
            return pictureFlavor.equals(flavor);
        }
    }
}

