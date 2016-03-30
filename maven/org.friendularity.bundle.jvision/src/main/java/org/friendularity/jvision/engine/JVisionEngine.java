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
import org.friendularity.jvision.filters.FilterSequence;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.highgui.Highgui;
import org.opencv.highgui.VideoCapture;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.friendularity.jvision.broker.SimpleImageStreamProducer;
import org.friendularity.jvision.gui.FileLocations;
import org.opencv.imgproc.Imgproc;

/**
 *  Core class that runs the jvision processing loop and publishes the
 * camera
 * 
 * @author Annie
 */
public class JVisionEngine extends BasicDebugger implements Runnable {
	public static final String CAMERA_NAME = "jvision.camera";
	private static JVisionEngine sDefaultJVisionEngine = null;
	private VideoCapture myVidCapture;
	private Mat myCameraImage_Mat;
	private SimpleImageStreamProducer myCameraISP = new SimpleImageStreamProducer(CAMERA_NAME);
	private final Object cameraToken = new Object();
	
	private Quitter myQuitter;

	public static JVisionEngine getDefaultJVisionEngine() {
		if (sDefaultJVisionEngine == null) {
			sDefaultJVisionEngine = new JVisionEngine();
		}

		return sDefaultJVisionEngine;
	}

	private JVisionEngine() {
		super();
		ImageStreamBroker.getDefaultImageStreamBroker().addImageStreamProducer(myCameraISP);
	}

	public void setQuitter(Quitter q) {
		myQuitter = q;
	}
	
	public void changeCamera(int camera) {
		getLogger().info("Changing vidCapture stream to " + camera);
		synchronized(cameraToken)
		{
			myVidCapture.release();
			myVidCapture = new VideoCapture();

			if (!myVidCapture.open(camera)) {
				getLogger().error("Failed to open vidCapture stream");
				myVidCapture = null;
			}
		}
	}

	public boolean connect() {

		boolean connectedFlag = false;
		try {
			getLogger().info("opening native library often takes 5-10 seconds");
			getLogger().info("Opening native library handle for OpenCV " + Core.VERSION);
			System.loadLibrary(Core.NATIVE_LIBRARY_NAME);

			synchronized(cameraToken)
			{
				myVidCapture = new VideoCapture();

				getLogger().info("Opening vidCapture stream");
				if (!myVidCapture.open(0)) {
					getLogger().error("Failed to open vidCapture stream");
					myVidCapture = null;
					return false;
				}
			}

			myCameraImage_Mat = new Mat();
			connectedFlag = true;
		} catch (Throwable t) {
			getLogger().error("Problem connecting JVisionEngine: ", t);
		}
		
		return connectedFlag;
	}

	// Without "synchronized", we are vulnerable to crash if displayerList modified during loop below.
	// Note that this method allows OpenCV exceptions to escape, such as 
	//	  Unknown exception in JNI code {Mat::n_1copyTo__JJ()} at org.opencv.core.Mat.n_copyTo(Native Method)
	
	private synchronized void processOneFrame() {
		VideoCapture vc = myVidCapture;
		synchronized(cameraToken)
		{
			if (!vc.read(myCameraImage_Mat)) {
				getLogger().error("Oops bad read");
			}
		}
		myCameraISP.setConsumedImage(new ImageStreamImage(myCameraImage_Mat));
		
		// Stu - I'm going to add a new, better timing display scheme, I had to remove this
		// hacky one to do it. I want to put in hooks that eventually will be some CV performance
		// profiler

		// yup, always suspcious doing this, but we don't have a sleep in here
		// and don't want to always lock up the process
		Thread.yield();
	}

	/**
	 * Converts/writes a Mat into a BufferedImage.
	 * This process takes a few msec for a 640x480 image on my desktop, so
	 * it should be avoided if at all possible
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

					raster.setSample(x, y , 0, b[x + y * width]);
				}
			}
		} else {
			int channels = bgr.channels();
			if (channels != 3) {
				throw new IllegalArgumentException("We only handle 3 channel images now");
			}

			image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
			raster = image.getRaster();
			// TODO:  Consider whether a reusable pool of these byte-buffers makes sense.
			byte[] b = new byte[width * height * channels];
			
/*  2016-03-29  Stu notes we are not seeing that error in current output from o.f.b.demo.ccmio.
          Don't recall if we conclusively fixed this problem across all cases between note
          below and Ann moving on to other work, but perhaps it is time to let this comment retire?

2013-12-30 - Stu notes that we often see (when running under CCRK):
 *     [java] java.lang.Exception: Unknown exception in JNI code {Mat::nGetB()}
     [java] 	at org.opencv.core.Mat.nGetB(Native Method)
     [java] 	at org.opencv.core.Mat.get(Mat.java:2551)
     [java] 	at org.friendularity.jvision.engine.JVisionEngine.matToBufferedImage(JVisionEngine.java:219)
     [java] 	at org.friendularity.jvision.engine.JVisionEngine.processOneFrame(JVisionEngine.java:163)
     [java] 	at org.friendularity.jvision.engine.JVisionEngine.run(JVisionEngine.java:256)
	 * 
	 Stu - clearly something isn't playing nice among OpenGL, OpenCV and Java.
	 
 */			
			bgr.get(0, 0, b);

			int[] rgb = new int[3];

			for (int y = 0; y < height; y++) {
				int base = y * width * channels;

				for (int x = 0; x < width; x++) {
          // This is the base of the performance issues with this routine

					// this operation is really inefficient!
					//  bgr.get(y,x,px);
          
          // this can be improved further by copying back out of the array in some
          // fancier way perhaps.
					rgb[0] = b[base + 3 * x + 2];
					rgb[1] = b[base + 3 * x + 1];
					rgb[2] = b[base + 3 * x];
					raster.setPixel(x, y, rgb);
				}
			}
		}

		return image;
	}

	@Override
	public void run() {

		if (myQuitter != null) {
			int MAX_ERRORS_ALLOWED = 100;
			int errorCount = 0;
			int framesProcessedCount = 0;
			getLogger().info("JVision Engine beginning frame processing loop");
			while (!myQuitter.wantsToQuit()) {
				//  Every N frames increment k, and print a "processed k*N frames so far" message at info() level.
				try {
					processOneFrame();
					framesProcessedCount++;
					if (framesProcessedCount % 1000 == 0) {
						getLogger().info("JVision system has now processed {} frames, and caught {} errors.", framesProcessedCount, errorCount);
					}
				} catch (Throwable t) {
					errorCount++;
					// TODO : set a "quit-reason" so outer layers can decide whether to try restart.
					getLogger().error("Exception # " + errorCount + " (out of " + 
							MAX_ERRORS_ALLOWED + " allowed) caught during processOneFrame", t);
					if (t instanceof InterruptedException) {
						getLogger().warn("JVision run() got InterruptedException, run loop will quit.");
						myQuitter.setWantsToQuit(true);
					}
					if (errorCount >= MAX_ERRORS_ALLOWED) {
						getLogger().warn("JVision run() error count is now {}, run loop will quit.", errorCount);
						myQuitter.setWantsToQuit(true);
					}
				}
			}
			getLogger().info("Quitter asked us to end the processing loop");
		} else {
			getLogger().warn("I don't have a quitter set, so I'm not going to run!");
		}
		getLogger().info("Releasing vidCapture");
		synchronized(cameraToken) {
			myVidCapture.release();
		}
		getLogger().info("run() complete, notifying quitter we're done");
		myQuitter.notifyQuitCompleted();
	}

	/**
	 * retained because it's a useful sample, and because the method name is memorable
	 * 
	 */
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

	public void requestQuit() {
		myQuitter.setWantsToQuit(true);
	}
}
