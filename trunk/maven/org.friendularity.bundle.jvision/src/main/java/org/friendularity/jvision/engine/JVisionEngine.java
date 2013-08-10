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
package org.friendularity.jvision.engine;

import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.util.ArrayList;
import java.util.Iterator;
import org.friendularity.jvision.filters.FilterSequence;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.highgui.Highgui;
import org.opencv.highgui.VideoCapture;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.jvision.gui.FileLocations;
import org.opencv.imgproc.Imgproc;
/**
 *
 * @author Owner
 */
public class JVisionEngine extends BasicDebugger implements Runnable {
  private static JVisionEngine sDefaultJVisionEngine = null;
  
	private VideoCapture	myVidCapture;
	private Mat				myCameraImage_Mat;
	private FilterSequence	myFilterSeq;
	private	ArrayList<Displayer>		myDisplayerList = new ArrayList<Displayer>();
	private Quitter			myQuitter;
  
  public static JVisionEngine getDefaultJVisionEngine() {
    if(sDefaultJVisionEngine == null)
      sDefaultJVisionEngine = new JVisionEngine();
    
    return sDefaultJVisionEngine;
  }
  
  private JVisionEngine() {
    super();
  }
	
	public FilterSequence getFilterSeq() { 
		return myFilterSeq;
	}
	public void addDisplayer(Displayer d) {
		myDisplayerList.add(d);
	}
	public void setQuitter(Quitter q) {
		myQuitter = q;
	}
	public boolean connect() {

		myFilterSeq = new FilterSequence();

		getLogger().info("Opening native library handle for OpenCV " + Core.VERSION);
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		Mat m = Mat.eye(3, 3, CvType.CV_8UC1);
		getLogger().info("m = " + m.dump());

		// testWithSomeDuckFiles();
		
		myVidCapture = new VideoCapture();

		getLogger().info("Opening vidCapture stream");
		if (!myVidCapture.open(0)) {
			getLogger().error("Failed to open vidCapture stream");
			myVidCapture = null;
			return false;
		}

		double w = myVidCapture.get(Highgui.CV_CAP_PROP_FRAME_WIDTH);
		double h = myVidCapture.get(Highgui.CV_CAP_PROP_FRAME_HEIGHT);

		System.out.println(Double.toString(w));

		myCameraImage_Mat = new Mat();

		return true;

	}

	public void processOneFrame() {
		VideoCapture vc = myVidCapture;
		if (!vc.read(myCameraImage_Mat)) {
			getLogger().error("Oops bad read");
		}

		long t = System.nanoTime();
		Mat filtered_camera_image = new Mat();
		myFilterSeq.apply(myCameraImage_Mat, filtered_camera_image);

    BufferedImage frame_as_buffered_image = null;
            
    for(Iterator<Displayer> i = myDisplayerList.iterator() ; i.hasNext(); )
    {
      if(frame_as_buffered_image == null) {
        frame_as_buffered_image = matToBufferedImage(filtered_camera_image);
      }
      i.next().setDisplayedImage(frame_as_buffered_image);
    }
				

		long new_t = System.nanoTime();

		double ns = (new_t - t) / 1000000000.0;  // frametime in sec
		double rate = 1.0 / ns;
    for(Iterator<Displayer> i = myDisplayerList.iterator() ; i.hasNext(); )
    {
      i.next().setFramerateMessage(String.format("%4.0f msec/frame, %5.1f frames per second",
				(1000.0 * ns),
				rate));
    }

		Thread.yield();
	}

	/**
	 * Converts/writes a Mat into a BufferedImage.
	 *
	 * @param bgr Mat of type CV_8UC3 or CV_8UC1
	 * @return BufferedImage of type TYPE_INT_RGB or TYPE_BYTE_GRAY
	 */
	public static BufferedImage matToBufferedImage(Mat bgr) {
		int width = bgr.width();
		int height = bgr.height();
		BufferedImage image;
		WritableRaster raster;

		if (bgr.channels() == 1) {
			image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY);
			raster = image.getRaster();
			byte[] b = new byte[width * height];
			bgr.get(0, 0, b);

			for (int y = 0; y < height; y++) {
				for (int x = 0; x < width; x++) {

					raster.setSample(x, y, 0, b[x + y * width]);
				}
			}
		} else {
			int channels = bgr.channels();
			if (channels != 3) {
				throw new IllegalArgumentException("We only handle 3 channel images now");
			}

			image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
			raster = image.getRaster();
			byte[] b = new byte[width * height * channels];
			bgr.get(0, 0, b);

			int[] rgb = new int[3];

			for (int y = 0; y < height; y++) {
				int base = y * width * channels;

				for (int x = 0; x < width; x++) {
					// this operation is really inefficient!
					//  bgr.get(y,x,px);
					rgb[0] = b[base + 3 * x + 2];
					rgb[1] = b[base + 3 * x + 1];
					rgb[2] = b[base + 3 * x];
					raster.setPixel(x, y, rgb);
				}
			}
		}

		return image;
	}

	@Override public void run() {
		if (myQuitter != null) {
			getLogger().info("JVision Engine beginning frame processing loop");
			while (!myQuitter.wantsToQuit()) {
				// TODO:  Every N frames increment k, and print a "processed k*N frames so far" message at info() level.
				processOneFrame();
			}
			getLogger().info("Quitter asked us to end the processing loop");
		} else {
			getLogger().warn("I don't have a quitter set, so I'm not going to run!");
		}
		getLogger().info("Releasing vidCapture");
		myVidCapture.release();
		getLogger().info("run() complete, notifying quitter we're done");
		myQuitter.notifyQuitCompleted();
	}
	
	public void testWithSomeDuckFiles() { 
		 Mat image = Highgui.imread(FileLocations.imageBase() + "duck.jpg", 1);
		 Mat grayimage = new Mat();
		 // this makes a new matrix
		 Imgproc.cvtColor(image, grayimage, Imgproc.COLOR_RGB2GRAY);
        
		 Highgui.imwrite(FileLocations.imageBase() + "grayduck.png", grayimage);
        
		 Mat someimage = new Mat();
        
        
		 image.convertTo(someimage, 0, 0.5);
		 Highgui.imwrite(FileLocations.imageBase() + "outduck.png", someimage);		
	}
}
