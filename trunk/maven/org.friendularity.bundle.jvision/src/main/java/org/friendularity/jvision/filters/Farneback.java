package org.friendularity.jvision.filters;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.Video;
import org.opencv.core.CvType;
import org.opencv.core.Point;
import org.opencv.core.Scalar;




public class Farneback implements BaseFilter {
    Mat mLastFrame = null;
    Mat mFlowImage = new Mat(640, 480, CvType.CV_32FC2);
	@Override
	public void apply(Mat in, Mat out) {
    Mat gray = new Mat();
		Imgproc.cvtColor(in, gray, Imgproc.COLOR_RGB2GRAY);
    
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
      /*      Core.rectangle(cflowmap, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height), new Scalar(0, 255, 0));
            line(cflowmap, Point(x,y), Point(cvRound(x+fxy.x), cvRound(y+fxy.y)),
                 color);
            circle(cflowmap, Point(x,y), 2, color, -1); */
        }
    }
    
    total_x = total_x / h / w * 256.0f;  // normalize to 1 point
    total_y = total_y / h / w * 256.0f;
    
    
    Core.line(cflowmap, new Point(320, 240), 
            new Point((int)(320 + 10 * scale * total_x),
                      (int)(240 + 10 * scale * total_y)), new Scalar(0, 255, 255), 4);
    
}

}
