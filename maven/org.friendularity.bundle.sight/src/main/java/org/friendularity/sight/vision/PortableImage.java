/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.sight.vision;

import java.awt.Component;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.MemoryImageSource;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class PortableImage implements Serializable {

	private static Logger theLogger = Logger.getLogger(PortableImage.class.getName());
	private transient Image myJavaImage;
	private transient OpenCVImage myOCVI;
	private int myWidth, myHeight;
	private byte[] myBytesWithoutHeader;

	public PortableImage() {
	}

	public PortableImage(Image awtImage, boolean flipVertical) {
		// This method is called from RawFrameProcessor.getPortableSubImage.
		// The resulting raw pixel-encoding bytes (with no BMP header) are easily
		// serialized to another process.
		// In that other process, we are able to construct an OpenCVImage using the bytes,
		// because apparently the OpenCVImage reading code doesn't require a BMP header.
		// We are also able to construct a new Java image using the readJavaImageFromBytes
		// method below.

		// We don't cache the supplied image, because it may need to be vertically
		// flipped, and also we want to be sure it is reconstituted properly
		// (so we avoid keeping an "original" around where it might be displayed
		// and cause confusion).
		// myJavaImage = awtImage;
		OpenCVImage.BufferGatherer b = new OpenCVImage.BufferGatherer(awtImage, flipVertical);
		setWidth(b.width());
		setHeight(b.height());
		setBytesWithoutHeader(b.bytes());
	}
	
	public PortableImage(BufferedImage img, Rectangle bbox, boolean flipVertical) {
		this(img.getSubimage(bbox.x, bbox.y, bbox.width, bbox.height), flipVertical);
	}

	public OpenCVImage fetchOpenCVImage() {
		if (myOCVI == null) {
			myOCVI = new OpenCVImage(myWidth, myHeight, myBytesWithoutHeader);
		}
		return myOCVI;
	}

	public byte[] getBytesWithoutHeader() {
		return myBytesWithoutHeader;
	}

	public void setBytesWithoutHeader(byte[] b) {
		// theLogger.info("Received byte array of length: " + b.length);
		myBytesWithoutHeader = b;
		// printDiag(10000);
	}
	public void printDiag(int step) {
		int len = myBytesWithoutHeader.length;
		for (int idx = 0; idx < len; idx += step) {
			theLogger.info("byte[" + idx + "] = " + myBytesWithoutHeader[idx]);
		}
		int idx = len -1;
		theLogger.info("byte[" + idx + "] = " + myBytesWithoutHeader[idx]);
	}

	public int getWidth() {
		return myWidth;
	}

	public int getHeight() {
		return myHeight;
	}

	public void setWidth(int w) {
		myWidth = w;
	}

	public void setHeight(int h) {
		myHeight = h;
	}

	public Image fetchJavaImage() {
		if (myJavaImage == null) {
			MemoryImageSource mis  = buildMemoryImageSourceForBytesWithoutHeader(myBytesWithoutHeader, myWidth, myHeight);
			myJavaImage = Toolkit.getDefaultToolkit().createImage(mis);
		}
		return myJavaImage;
	}
	public Image fetchJavaImage(Component comp) {
		return fetchJavaImage();
	}

	private static Image readJavaImageFromBGBytes(Component comp, byte[] iba, int w, int h) {
		// Was this tried and gave error?    Why is comp approach better?
		// Image img = Toolkit.getDefaultToolkit().createImage(mis);
		MemoryImageSource mis  = buildMemoryImageSourceForBytesWithoutHeader(iba, w, h);
		Image img = comp.createImage(mis);
		return img;
	}

	private static MemoryImageSource buildMemoryImageSourceForBytesWithoutHeader(byte[] iba, int w, int h) {
		// iba is a bitmap byte array with no header produced by OpenCVImage.BufferGatherer.
		// We must be careful because java treates byte as a SIGNED type,
		// but we need to work with this data as unsigned bits.
		// Hence the masking as we convert from byte to int.
		// See the sample code in Javadocs for MemoryImageSource.
		int pixArray[] = new int[w * h];
		int pixIndex = 0;
		for (int y = 0; y < h; y++) {
			for (int x = 0; x < w; x++) {
				int byteOffset = pixIndex * 3;
				int blue = iba[byteOffset] & 0xff;
				int green = iba[byteOffset + 1] & 0xff;
				int red = iba[byteOffset + 2] & 0xff;
				int alpha = 255;
				int argb = (alpha << 24) | (red << 16) | (green << 8) | blue;
				pixArray[pixIndex++] = argb;

			}
		}
		MemoryImageSource mis = new MemoryImageSource(w, h, pixArray, 0, w);
		return mis;
	}
}
		/*
		 * We have native code to write the bmp header in:
		 * JNIVisionFacade_RawVision.cpp-> writeBmpHeader()
		 *
		 * Our native code for reading these image bytes is in
		 * com_hansonrobotics_vision_OpenCVImage_createImageFromDataNative
		 */
