/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.gui.egosphere;


import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.person.PersonHelpFuncs;
import org.friendularity.app.person.PersonResolver;
import org.friendularity.app.person.PersonTracker;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.friendularity.gaze.estimate.GazeDirectionComputer;
import org.cogchar.api.animoid.gaze.IGazeTarget;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.SmallAngle;
import org.cogchar.api.integroid.cue.PersonCue;
import org.cogchar.api.integroid.cue.SightAttentionStatus;
import org.cogchar.sight.api.core.SightPort;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.freckler.sight.impl.hypo.SightModel;
import org.cogchar.sight.api.obs.IRawFrameObserver;
import org.cogchar.sight.api.obs.PortableImage;

import org.friendularity.gaze.api.AnimoidGazeFacade;

/**
 *
 * @author Matthew Stevenson
 */
public class EgosphereSwingPanel extends javax.swing.JPanel {

	private static Logger theLogger = Logger.getLogger(EgosphereSwingPanel.class.getName());
	private IRawFrameObserver myImageSource;
	private IntegroidFacade myIGF;
	private AnimoidGazeFacade myAF;
	private Double myMinAzDeg, myMaxAzDeg;
	private Double myMinElDeg, myMaxElDeg;
	private Double myScaleX, myScaleY;
	private Double myFoVCenterAzDeg, myFoVCenterElDeg;
	private Double myFoVMinAzDeg, myFoVMaxAzDeg;
	private Double myFoVMinElDeg, myFoVMaxElDeg;
	private Double myViewportAzDeg, myViewportElDeg;
	private Integer myFoVPanelMinX, myFoVPanelMaxX;
	private Integer myFoVPanelMinY, myFoVPanelMaxY;

	public boolean setIGF(IntegroidFacade igf) {
		myIGF = igf;
		if (myIGF != null && myIGF.getAnimoidFacade() != null) {
			myAF = (AnimoidGazeFacade) myIGF.getAnimoidFacade();
			return true;
		}
		return false;
	}

	public void setImageSource(IRawFrameObserver src) {
		myImageSource = src;
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		if (myImageSource != null) {
			drawImage(g);
		}
	}

	public void drawImage(Graphics g) {
		Image img = myImageSource.getImage();
		if (!updateRangeVars() || myFoVPanelMinX == null) {
			theLogger.severe("Unable to update range values.");
			return;
		}
		if (img != null) {
			try {
				g.drawImage(img, myFoVPanelMinX, myFoVPanelMinY, myFoVPanelMaxX, myFoVPanelMaxY,
					0, 0, img.getWidth(null), img.getHeight(null), null);
				drawGridLines(g);
				drawPersonTrackerAnnotations(g);

			} catch (Throwable t) {
				theLogger.log(Level.SEVERE, "EgosphereSwingPanel caught exception", t);
			}
		} else {
			theLogger.warning("EgosphereSwingPanel is not drawing, because getImage() yields null");
		}
	}

	public boolean updateRangeVars() {
		if (myAF == null) {
			theLogger.info("Animoid Facade is null, cannot continue.");
			return false;
		}
		SightModel sm = myAF.getSightModel();
		if (sm == null) {
			theLogger.info("Sight Model is null, cannot continue.");
			return false;
		}
		GazeDirectionComputer gdc = (GazeDirectionComputer) sm.getGazeDirectionComputer();
		if (gdc == null) {
			theLogger.info("Gaze Direction Computer is null, cannot continue.");
			return false;
		}
		EgocentricDirection ed = myAF.getCurrentEgocentricDirection();
		if (ed == null) {
			theLogger.info("EgocentricDirection is null, cannot continue.");
			return false;
		}
		SightPort vp = gdc.getViewPort();
		myViewportAzDeg = vp.getWidthAngle().getDegrees();
		myViewportElDeg = vp.getHeightAngle().getDegrees();

		myMinAzDeg = gdc.getMinViewableAzimuthDeg();
		myMaxAzDeg = gdc.getMaxViewableAzimuthDeg();
		myMinElDeg = gdc.getMinViewableElevationDeg();
		myMaxElDeg = gdc.getMaxViewableElevationDeg();

		Double azRangeDeg = myMaxAzDeg - myMinAzDeg;
		myScaleX = getWidth() / azRangeDeg;

		Double yRangeDeg = myMaxElDeg - myMinElDeg;
		myScaleY = getHeight() / yRangeDeg;


		myFoVCenterAzDeg = ed.getAzimuth().getDegrees();
		myFoVCenterElDeg = ed.getElevation().getDegrees();

		SmallAngle centerAz = SmallAngle.makeFromDeg(myFoVCenterAzDeg);
		SmallAngle centerEl = SmallAngle.makeFromDeg(myFoVCenterElDeg);

		myFoVMaxAzDeg = vp.getMaxViewableAzForCenter(centerAz).getDegrees();
		myFoVMinAzDeg = vp.getMinViewableAzForCenter(centerAz).getDegrees();
		myFoVMaxElDeg = vp.getMaxViewableElForCenter(centerEl).getDegrees();
		myFoVMinElDeg = vp.getMinViewableElForCenter(centerEl).getDegrees();

		myFoVPanelMaxX = convertToPanelCoords(myFoVMaxAzDeg, myScaleX, myMinAzDeg);
		myFoVPanelMinX = convertToPanelCoords(myFoVMinAzDeg, myScaleX, myMinAzDeg);
		myFoVPanelMaxY = getHeight() - convertToPanelCoords(myFoVMaxElDeg, myScaleY, myMinElDeg);
		myFoVPanelMinY = getHeight() - convertToPanelCoords(myFoVMinElDeg, myScaleY, myMinElDeg);


		return true;
	}

	/*
	private Frame getRangeDiagonalJointPositionFrame(Frame center, double fractionalRomPosOnDiagonal) {
	Frame f = new Frame();
	List<JointPosition> jps = center.getAllPositions();
	for (JointPosition jp : jps) {
	JointPosition newJp = jp.convertToCooordinateType(JointStateCoordinateType.FLOAT_ABS_RANGE_OF_MOTION);
	newJp.setCoordinateFloat(JointStateCoordinateType.FLOAT_ABS_RANGE_OF_MOTION, fractionalRomPosOnDiagonal);
	f.addPosition(newJp);
	}
	return f;
	}
	 */
	private Integer convertToPanelCoords(Double angle, Double scale, Double min) {
		return (int) (scale * (angle - min));
	}

	private Point getPanelPointForDirection(EgocentricDirection ed) {
		double azDeg = ed.getAzimuth().getDegrees();
		double elDeg = ed.getElevation().getDegrees();
		return getPanelPointForDirection(azDeg, elDeg);
	}

	private Point getPanelPointForDirection(double azDeg, double elDeg) {
		int x = convertToPanelCoords(azDeg, myScaleX, myMinAzDeg);
		int y = getHeight() - convertToPanelCoords(elDeg, myScaleY, myMinElDeg);
		Point p = new Point(x, y);
		return p;
	}

	private int gridSideSegmentCount(double valueExtent, double gridSpacing) {
		return (int) Math.floor(Math.abs(valueExtent) / gridSpacing);
	}

	private void drawGridLines(Graphics g) throws Throwable {
		double gridSpacing = 10.0;
		int leftSegCount = gridSideSegmentCount(myMinAzDeg, gridSpacing);
		int rightSegCount = gridSideSegmentCount(myMaxAzDeg, gridSpacing);
		int botSegCount = gridSideSegmentCount(myMinElDeg, gridSpacing);
		int topSegCount = gridSideSegmentCount(myMaxElDeg, gridSpacing);

		g.setColor(Color.BLUE);
		for (int hgi = -1 * leftSegCount; hgi <= rightSegCount; hgi++) {
			double azDeg = hgi * gridSpacing;
			Point topPoint = getPanelPointForDirection(azDeg, myMaxElDeg);
			Point botPoint = getPanelPointForDirection(azDeg, myMinElDeg);
			g.setColor(Color.BLUE);
			g.drawLine(topPoint.x, topPoint.y, botPoint.x, botPoint.y);
			g.setColor(Color.YELLOW);
			g.drawString("" + azDeg, topPoint.x - 10, topPoint.y + 15);

		}
		for (int vgi = -1 * botSegCount; vgi <= topSegCount; vgi++) {
			double elDeg = vgi * gridSpacing;
			Point leftPoint = getPanelPointForDirection(myMinAzDeg, elDeg);
			Point rightPoint = getPanelPointForDirection(myMaxAzDeg, elDeg);
			g.setColor(Color.BLUE);
			g.drawLine(leftPoint.x, leftPoint.y, rightPoint.x, rightPoint.y);
			g.setColor(Color.YELLOW);
			g.drawString("" + elDeg, leftPoint.x + 5, leftPoint.y + 2);
		}

	}

	private void drawPersonTrackerAnnotations(Graphics g) throws Throwable {
		PersonResolver pr = PersonHelpFuncs.getPersonResolver(myIGF);
		Set<PersonTracker> allTrackers = pr.getAllTrackers();

		IGazeTarget igt = myAF.getAttentionTarget();

		// First draw all trackers:
		for (PersonTracker pt : allTrackers) {
			drawPersonTracker(g, pt);
		}
		// Now redraw the "active" blue ones on top.
		for (PersonTracker pt : allTrackers) {
			SightAttentionStatus attentionStatus = null;
			if (pt != null) {
				PersonCue pc = pt.getCue();
				if (pc != null) {
					attentionStatus = pc.getAttentionStatus();
					if ((attentionStatus != null) && (attentionStatus != SightAttentionStatus.IGNORED)) {
						drawPersonTracker(g, pt);
					}
				}
			}

		}

	}

	private void drawPersonTracker(Graphics g, PersonTracker pt) {
		boolean gazeHoldFlag = myAF.getAttentionJob().getHoldingStatusFlag();
		SightAttentionStatus attentionStatus = null;
		if (pt != null) {
			PersonCue pc = pt.getCue();
			if (pc != null) {
				attentionStatus = pc.getAttentionStatus();
			}
		}

		FaceObservation fo = pt.getMostAccurateObservation();
		if (fo != null) {
			EgocentricDirection ed = fo.getCenterDirection();
			Point panelCenterPoint = getPanelPointForDirection(ed);
			PortableImage flippedFacePortableImage = fo.getFaceImage();
			if (flippedFacePortableImage == null) {
				theLogger.warning("FaceObs returned null face portableImage");
				return;
			}
			Image faceImage = flippedFacePortableImage.fetchJavaImage(this);

			if (faceImage != null) {
				int faceWidth = faceImage.getWidth(null);
				int faceHeight = faceImage.getHeight(null);
				double scaleFactor = 40.0 / faceHeight;
				int scaledWidth = (int) Math.floor(faceWidth * scaleFactor);
				int scaledHeight = (int) Math.floor(faceHeight * scaleFactor);

				int panelX = panelCenterPoint.x - scaledWidth / 2;
				int panelY = panelCenterPoint.y - scaledHeight / 2;
				g.drawImage(faceImage, panelX, panelY, panelX + scaledWidth,
					panelY + scaledHeight, 0, 0, faceWidth, faceHeight, null);
				if ((attentionStatus != null) && (attentionStatus != SightAttentionStatus.IGNORED)) {
					Color targetMarkerColor;
					if (gazeHoldFlag) {
						targetMarkerColor = new Color(0.5f, 0.5f, 1.0f, 0.6f);
					} else {
						targetMarkerColor = new Color(0.0f, 0.0f, 1.0f, 0.6f);
					}
					g.setColor(targetMarkerColor);
					g.fillOval(panelX, panelY, scaledWidth, scaledHeight);
				}
				g.setColor(Color.YELLOW);
				int textX = panelX;
				int textY = panelY + scaledHeight + 10;
				g.drawString("h=" + pt.getHypoNumber(), textX, textY);
				textY += 15;
				g.drawString("c=" + pt.getCueShortDesc(), textX, textY);
				textY += 15;
				g.drawString("e=" + PersonHelpFuncs.formattedAttentionEligibility(pt.getCue()), textX, textY);
			}
		} else {
			theLogger.warning("Can't get most accurate obs for: " + pt);
		}

	}
}
		