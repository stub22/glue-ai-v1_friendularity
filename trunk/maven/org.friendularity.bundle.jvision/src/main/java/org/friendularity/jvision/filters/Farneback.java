package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.Video;
import org.opencv.core.CvType;
import org.opencv.core.Point;
import org.opencv.core.Scalar;




public class Farneback implements BaseFilter {
    Mat mLastFrame = null;
    Mat mFlowImage = null; 
	@Override
	public void apply(Mat in, Mat out) {
		if(mFlowImage == null)
			mFlowImage = new Mat(in.width(), in.height(), CvType.CV_32FC2);
		
		Mat gray = new Mat();
		if(in.channels() > 1) 
			Imgproc.cvtColor(in, gray, Imgproc.COLOR_RGB2GRAY);
		else
			in.copyTo(gray);

		// first frame
		if(mLastFrame == null)
		{
		  mLastFrame = new Mat();
		  Imgproc.cvtColor(in, mLastFrame, Imgproc.COLOR_RGB2GRAY);
		  in.copyTo(out);
		  return;
		}

		Video.calcOpticalFlowFarneback(mLastFrame, gray, mFlowImage, 0.5, 3, 15, 3, 5, 1.2, 0);

		in.copyTo(out);
		drawOptFlowMap(mFlowImage, out);
		gray.copyTo(mLastFrame);
	}
  
	/**
	 * 
	 * @param flow       CV_32FC2 image
	 * @param cflowmap   rgb image to draw the lines into
	 */
  static void drawOptFlowMap(Mat flow, Mat cflowmap)
	{
		int h = cflowmap.height();
		int w = cflowmap.width();
		float[] fbuf = new float[2];
		float scale = 1.0f;
		float total_x = 0.0f;
		float total_y = 0.0f;
		for(int y = 0; y < h; y += 16)
		{
			for(int x = 0; x < w; x += 16)
			{
				flow.get(x,y, fbuf);
				// TODO understand this float structure better
				Point fp = new Point(fbuf[0],fbuf[1]);
				total_x += fp.x;
				total_y += fp.y;

				Core.line(cflowmap, new Point(x, y), new Point((int)(x + scale * fp.x), (int)(y + scale * fp.y)), new Scalar(128, 255, 128));
				Core.circle(cflowmap, new Point(x, y), 2, new Scalar(128, 128, 128), -1);
			}
		}

		total_x = total_x / h / w * 256.0f;  // normalize to 1 point
		total_y = total_y / h / w * 256.0f;

		Core.line(cflowmap, new Point(w / 2.0, h / 2.0), 
				new Point((int)(w / 2.0 + 10 * scale * total_x),
						  (int)(h / 2.0 + 10 * scale * total_y)), new Scalar(0, 255, 255), 4);
	}

	@Override
	public String toString() {
		return "farneback_optical_flow"; 
	}

	@Override
	public void showParamUI(JFrame parent) {
		
	}

	@Override
	public String serialize() {
		return "";
	}

	@Override
	public void deserialize(String str) {
		
	}
}
