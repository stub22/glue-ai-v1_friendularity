package org.friendularity.jvision.gui;

import org.friendularity.jvision.gui.FileLocations;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.util.ArrayList;
import java.util.Formatter;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.highgui.*;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.Video;
import org.opencv.utils.Converters;
import org.friendularity.jvision.gui.DemoFrame;
import org.friendularity.jvision.filters.FilterSequence;

public class JVisionLauncher {

    public static void main(String[] args) {
    	DemoFrame df = new DemoFrame();
    	FilterSequence filters = new FilterSequence();
    	df.setControlledFilterSequence(filters);
    	
        System.out.println("Welcome to OpenCV " + Core.VERSION);
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
        Mat m  = Mat.eye(3, 3, CvType.CV_8UC1);
        System.out.println("m = " + m.dump());
        
        Mat image = Highgui.imread(FileLocations.imageBase() + "duck.jpg", 1);
        Mat grayimage = new Mat();
        // this makes a new matrix
        Imgproc.cvtColor(image, grayimage, Imgproc.COLOR_RGB2GRAY);
        
        Highgui.imwrite(FileLocations.imageBase() + "grayduck.png", grayimage);
        
        Mat someimage = new Mat();
        
        
        image.convertTo(someimage, 0, 0.5);
       Highgui.imwrite(FileLocations.imageBase() + "outduck.png", someimage);
       
       VideoCapture vc = new VideoCapture();
       
       if(!vc.open(0))
    	   System.out.println("oops problem opening");
       
       double w = vc.get(Highgui.CV_CAP_PROP_FRAME_WIDTH);
       double h = vc.get(Highgui.CV_CAP_PROP_FRAME_HEIGHT);
       
       System.out.println(Double.toString(w));
       
       Mat camera_image = new Mat();
       
       while(!df.wantsToQuit())
       {   

	       
	       if (!vc.read(camera_image) ) System.out.println("Oops bad read");
	       
	       long t = System.nanoTime();
	       Mat filtered_camera_image = new Mat();
	       filters.apply(camera_image, filtered_camera_image);
	       

	       
	       df.setImage(matToBufferedImage(filtered_camera_image));
	       
	       long new_t = System.nanoTime();
	       
	       double ns = (new_t - t) / 1000000000.0;  // frametime in sec
	       double rate = 1.0 / ns;
	       
	       df.setFramerateMessage(String.format("%4.0f msec/frame, %5.1f frames per second", 
	    		   (1000.0 * ns),
	    		   rate));

			Thread.yield();

       }
       vc.release();
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
        
        if (bgr.channels()==1) {
            image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY);
            raster = image.getRaster();
            byte [] b = new byte[width * height]; 
            bgr.get(0,0,b);

            for (int y=0; y<height; y++) {
                for (int x=0; x<width; x++) {
                   
                    raster.setSample(x, y, 0, b[x + y * width]);
                }
            }
        } else {
        	int channels = bgr.channels();
        	if (channels != 3)throw new IllegalArgumentException("We only handle 3 channel images now");
        	
            image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            raster = image.getRaster();
            byte [] b = new byte[width * height * channels]; 
            bgr.get(0,0,b);

            int[] rgb = new int[3];

            for (int y=0; y<height; y++) {
            	int base = y * width * channels;
            	
                for (int x=0; x<width; x++) {
                	// this operation is really inefficient!
                  //  bgr.get(y,x,px);
                    rgb[0] = b[base + 3 * x + 2];
                    rgb[1] = b[base + 3 * x + 1];
                    rgb[2] = b[base + 3 * x];
                    raster.setPixel(x,y,rgb);
                }
            }
        }

        return image;
    }
}
