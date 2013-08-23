/*
 * OpenCVImage.java
 * 
 * Created on Oct 24, 2007, 2:37:24 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.util.Hashtable;

import java.util.logging.Logger;

/**
 *
 * @author josh N stu
 */
public class OpenCVImage {
	
	private static Logger	theLogger = Logger.getLogger(OpenCVImage.class.getName());
	/* Unused
    public OpenCVImage(long pointer) {
        m_ptr = pointer;
        m_image = null;
    }
	*/
	
	private static long numConstructed = 0;
	private static long numFinalized = 0;

    private native void cleanUpNative(long addr);
    private native long createImageFromDataNative(int width, int height, byte[] b);
  
    private native int saveFileNative(long addr, String filename);
	private native long createImageFromFile(String filename, int colorFlags);

	// Unused and untested:
	private native int[] findFacesNative(long addr, String config);
	// Native implementation of getImageDataNative() is not complete.
	// **
    // private native byte[] getImageDataNative();
	// **

	// Pointer to native memory.
    private long m_ptr;

    public OpenCVImage(int width, int height, byte[] bytes) {
        m_ptr = createImageFromDataNative(width, height, bytes);
		numConstructed++;
		theLogger.finer("Constructed OpenCVImage #" +  numConstructed +  " from byte array");
	}
	public OpenCVImage(String filename, int colorFlags) {
		m_ptr = createImageFromFile(filename, colorFlags);
		numConstructed++;
		theLogger.finer("Constructed OpenCVImage #" +  numConstructed +  " from file: " + filename);
	}
    protected void finalize() {
    	cleanUpNative(m_ptr);
		numFinalized++;
		theLogger.finer("Finalized OpenCVImage #" +  numFinalized);
    }
	/* Unused, and depends on unimplemented method getImageDataNative().
    public Image image() {
        if (m_image == null) {
            // get the data buffer from the C++ side
            byte[] image_data = getImageDataNative();

            Iterator readers = ImageIO.getImageReadersByFormatName("bmp");
            ImageReader reader = (ImageReader)readers.next();
            AffineTransform transform = new AffineTransform();
            transform.scale(1.0, -1.0);

           try {
            	ImageInputStream iis = ImageIO.createImageInputStream(image_data);
            	reader.setInput(iis, true);
            	m_image = reader.read(0);
            } catch(IOException e) {
            }
        }

        return m_image;
    }
	*/

    public Rectangle[] findFaces(String config) {
		// Unused.  Does this work?
    	int[] rawFaces = findFacesNative(m_ptr, config);
    	int nFaces = rawFaces.length / 4;
    	Rectangle[] faces = new Rectangle[nFaces];
    	for ( int i = 0; i < nFaces; i++ )
    	{
    		int base = i*4;
    		faces[i] = new Rectangle(rawFaces[base], rawFaces[base+1],
    								rawFaces[base+2], rawFaces[base+3]);
    	}
    	return faces;
    }
    
    public void SaveFile(String filename) {
    	int retval = saveFileNative(m_ptr, filename);
		if (retval != 1) {
			throw new RuntimeException("saveFileNative[" + m_ptr + ", " + filename + " returned " + retval);
		}
    }
    
    public long raw() {
    	return m_ptr;
    }
    

	public static class BufferGatherer implements ImageConsumer {
		private boolean		myFlipVerticalFlag;
		public BufferGatherer(Image image, boolean flipVertical) {
			myFlipVerticalFlag = flipVertical;
			image.getSource().startProduction(this);
		}
		/* Unused
		public BufferGatherer(byte[] bytes) {

		}
		 */

		public void imageComplete(int status) {
			theLogger.finer("imageComplete status=" + status);
		}

		public void setColorModel(ColorModel model) {
			theLogger.finer("ColorModel=" + model);
			//  ColorModel=DirectColorModel: rmask=ff0000 gmask=ff00 bmask=ff amask=ff000000
		}

		public void setDimensions(int width, int height) {
			m_bytes = new byte[width*height*3];
			m_width = width;
			m_height = height;
		}

		public void setHints(int hintflags) {
		}

		public void setPixels(int x, int y, int w, int h,
				ColorModel model, byte[] pixels, int off, int scansize) {

			for ( int m = 0; m < w; m++ ) {
				for ( int n = 0; n < h; n++ ) {
					int mappedY = y;
					if (myFlipVerticalFlag) {
						mappedY = m_height-(y+n)-1;
					}
					int new_location = ( mappedY * m_width + (x+m) ) * 3;
					int orig_location = n*scansize + m + off;

					// Stu asks - is this approach to pixel extraction correct?
					// We must remember that bytes are *signed* 8 bit values!
					m_bytes[new_location+0] = (byte)model.getBlue(pixels[orig_location]);
					m_bytes[new_location+1] = (byte)model.getGreen(pixels[orig_location]);
					m_bytes[new_location+2] = (byte)model.getRed(pixels[orig_location]);
				}
			}
		}

		public void setPixels(int x, int y, int w, int h,
				ColorModel model, int[] pixels, int off, int scansize)
		{
			setPixelsPrivate(x, y, w, h, model, pixels, off, scansize);
		}

		private synchronized void setPixelsPrivate(int x, int y, int w, int h,
				ColorModel model, int[] pixels, int off, int scansize) {
			for ( int m = 0; m < w; m++ ) {
				for ( int n = 0; n < h; n++ ) {
					int mappedY = y;
					if (myFlipVerticalFlag) {
						mappedY = m_height-(y+n)-1;
					}
					int new_location = ( mappedY * m_width + (x+m) ) * 3;
					int orig_location = n*scansize + m + off;
					
					// Stu asks - is this approach to pixel extraction correct?
					// We must remember that bytes are *signed* 8 bit values!
					m_bytes[new_location+0] = (byte)model.getBlue(pixels[orig_location]);
					m_bytes[new_location+1] = (byte)model.getGreen(pixels[orig_location]);
					m_bytes[new_location+2] = (byte)model.getRed(pixels[orig_location]);
				}
			}
		}

		public void setProperties(Hashtable props) {
		}

		byte[] bytes() {
			return m_bytes;
		}

		int width() { return m_width; }
		int height() { return m_height; }

		private byte[] m_bytes;
		private int m_width;
		private int m_height;
	}
}