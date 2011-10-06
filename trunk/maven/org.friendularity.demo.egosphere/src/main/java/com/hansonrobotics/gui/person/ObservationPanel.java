/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package com.hansonrobotics.gui.person;

import java.awt.Graphics;
import java.awt.Image;
import java.util.logging.Logger;
import org.cogchar.vision.PortableImage;
import org.friendularity.app.face.FaceObservation;

/**
 *
 * @author StuB
 */
public class ObservationPanel extends javax.swing.JPanel  {
	private static Logger	theLogger = Logger.getLogger(ObservationPanel.class.getName());

	private FaceObservation		myFaceObs;

	public void setObservation(FaceObservation obs) {
		myFaceObs = obs;
	}


	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		if (myFaceObs == null) {
			theLogger.finer("Null observation found in paintComponent()");
			return;
		}
		PortableImage flippedFacePortableImage = myFaceObs.getFaceImage();
		if (flippedFacePortableImage == null) {
			theLogger.warning("FaceObs returned null face portableImage");
			return;
		}
		Image faceImage = flippedFacePortableImage.fetchJavaImage(this);
		boolean flipped = false;

		if (faceImage != null) {
            try {
				int faceWidth = faceImage.getWidth(null);
				int faceHeight = faceImage.getHeight(null);
				double scaleFactor = 40.0 / faceHeight;
				int scaledWidth = (int) Math.floor(faceWidth * scaleFactor);
				int scaledHeight = (int) Math.floor(faceHeight * scaleFactor);
				// Draws vertically inverted image at 1:1 scale
				// dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2,
				int yFirst, yLast;
				if (flipped) {
					yFirst = scaledHeight; yLast = 0;
				} else {
					yFirst = 0;  yLast = scaledHeight;
				}
				g.drawImage(faceImage,  0, yFirst, scaledWidth, yLast,
		                        0, 0, faceWidth, faceHeight, null);

				PortableImage shirtPI = myFaceObs.getShirtSampleImage();
				if (shirtPI != null) {
					int shirtDrawX = scaledWidth + 10;
					Image shirtJI = shirtPI.fetchJavaImage(this);
					g.drawImage(shirtJI, shirtDrawX, 0, null);
				}
				/*
				for (IAnnotatingObserver iao : m_annotaters) {
					iao.Annotate(g);
				}
				*/
            } catch (Exception e) {
				System.out.println("ObservationPanel caught exception: " + e);
				e.printStackTrace();
            }
		}
	}
}
