package org.friendularity.jvision.filters;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Contour implements BaseFilter {

	@Override
	public void apply(Mat in, Mat out) {
		try {
			List<MatOfPoint> contours = new ArrayList<MatOfPoint>();
			Mat hierarchy = new Mat();
			
			Imgproc.findContours(in, contours, hierarchy, Imgproc.RETR_EXTERNAL ,Imgproc.CHAIN_APPROX_TC89_KCOS );

			in.clone().copyTo(out);

			Scalar color = new Scalar(0.0f, 0.0f, 255.0f);
			
			// Draw a bounding box around each face.
			for (Iterator<MatOfPoint> mop = contours.iterator() ; mop.hasNext() ;) {
				MatOfPoint pts = mop.next();
				
				for(int r = 0 ; r < pts.rows() ; r++)
				{
					for(int c = 0 ; c < pts.cols() - 1 ; c++)
					{
						
						Core.line(out, new Point(pts.get(r, c)[0], pts.get(r, c)[1]), 
								new Point(pts.get(r, c + 1)[0], pts.get(r, c + 1)[1]), color);
					}
				}
				
			}
		} catch (Throwable t) {
			t.printStackTrace();
			in.clone().copyTo(out);
		}
	}
}
