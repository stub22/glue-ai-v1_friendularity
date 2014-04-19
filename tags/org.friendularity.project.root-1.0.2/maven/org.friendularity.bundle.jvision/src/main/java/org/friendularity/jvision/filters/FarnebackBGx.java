package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.Video;
import org.opencv.core.CvType;
import org.opencv.core.Point;
import org.opencv.core.Scalar;

public class FarnebackBGx extends ApplicativeBaseFilter {

	private static byte toIntRange(float f) {
		float ff = 128 + 8 * f;
		if(ff < 0.0f)ff = 0.0f;
		if(ff > 255.0f)ff = 255.0f;
		
		return (byte)ff;
	}
	
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
		drawFlowBGx(mFlowImage, out);
		gray.copyTo(mLastFrame);
	}
  
	/**
	 * 
	 * @param float32image    the CV_32FC2 flow map
	 * @param bgr8image a representation of the vertical speed of it
	 */
  static void drawFlowBGx(Mat float32image, Mat bgr8image)
	{
		int h = bgr8image.height();
		int w = bgr8image.width();
		float[] fbuf = new float[2];
		float scale = 1.0f;
		float total_x = 0.0f;
		float total_y = 0.0f;
		int inchannels = float32image.channels();
		float[] inbuf = new float[(int)float32image.total() * inchannels];
		float32image.get(0, 0, inbuf);
		int outchannels = bgr8image.channels();
		byte[] outbuf = new byte[(int)bgr8image.total() * outchannels];
		
		
		for(int y = 0; y < h; y += 1)
		{
			for(int x = 0; x < w; x += 1)
			{
				outbuf[(w * y + x) * outchannels] = toIntRange(inbuf[(w * y + x) * inchannels]);
				outbuf[(w * y + x) * outchannels + 1] = toIntRange(inbuf[(w * y + x) * inchannels + 1]);
				outbuf[(w * y + x) * outchannels + 2] = 0;
			}
		}

		bgr8image.put(0, 0, outbuf);

	}

	@Override
	public String toString() {
		return "farneback_BGx"; 
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
