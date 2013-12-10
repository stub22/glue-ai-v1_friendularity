/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.vision;


import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.jmxwrap.SignalStation;
import org.friendularity.app.person.PersonHelpFuncs;
import org.friendularity.app.person.PersonTracker;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.animoid.calc.estimate.GazeDirectionComputer;
import org.cogchar.api.sight.SightPort;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.api.integroid.cue.PersonCue;
import org.cogchar.sight.api.core.SightObservation;
import org.cogchar.api.sight.SightAttentionStatus;
import org.cogchar.sight.api.obs.IAnnotatingObserver;
import org.cogchar.sight.impl.hypo.SightHypothesis;


/**
 * @author Stu Baurmann
 */
public class FaceModelAnnotater implements IAnnotatingObserver {
	private static Logger	theLogger = Logger.getLogger(FaceModelAnnotater.class.getName());
	
	private		FaceModel	myFaceModel;
	
	private		Color		myColor = null;
	
	public FaceModelAnnotater(FaceModel fm) {
		myFaceModel = fm;
	}
	
	public synchronized void setDrawColor(Color c) {
		myColor = c;
	}
    public synchronized void Annotate(Graphics g) {
		// if (true) { return; }
		try {
			
			GazeDirectionComputer gazeDirectionComputer = myFaceModel.getGazeDirectionComputer();
			if (gazeDirectionComputer != null) {
				SightPort vp = gazeDirectionComputer.getViewPort();
				Collection<FaceHypothesis> hypos = myFaceModel.getHypoSnapshotOrderedByNum();
				for (SightHypothesis h : hypos) {
					drawSightHypothesisInfo(h, vp, g);
				}		
				drawCameraDirectionInfo(vp, g);
			}
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "g=" + g, t);
		}
	}
	private void drawSightHypothesisInfo(SightHypothesis h, SightPort vp, Graphics g) {
		// SightObservation obs = h.getMostRecentObservation();
		IntegroidFacade igf = SignalStation.getSignalStation().getIntegroidFacade();
		PersonTracker pt = PersonHelpFuncs.getPersonTrackerForHypo(igf, (FaceHypothesis) h);
		SightAttentionStatus attentionStatus = null;
		if (pt != null) {
			PersonCue pc = pt.getCue();
			if (pc != null) {
				attentionStatus = pc.getAttentionStatus();
			}
		}
		SightObservation obs = h.getMostAccurateObservation();
		if (obs == null) {
			theLogger.warning("Got null most-accurate-obs for hypo " + h);
			return;
		}
		double strength = h.getStrength();
		FaceHypothesis.ActivationStatus actStat = h.getActivationStatus();
		Color hypoColor = null;
		if ((attentionStatus != null) && (attentionStatus != SightAttentionStatus.IGNORED)) {
			// GazeTarget = Blue
			boolean gazeHoldFlag = igf.getAnimoidFacade().getAttentionJob().getHoldingStatusFlag();
			if (gazeHoldFlag) {
				// Light blue = gaze hold
				hypoColor = new Color(0.5f, 0.5f, 1.0f, (float) strength);
			} else {
				// Dark blue = goto gaze
				hypoColor = new Color(0.0f, 0.0f, 1.0f, (float) strength);
			}
		} else if (actStat == FaceHypothesis.ActivationStatus.POSTED) {
			// PersonCue, not gazeTarget = Green
			hypoColor = new Color(0.0f, 1.0f, 0.0f, (float) strength);
		} else {
			// Not person cue = Purple
			hypoColor = new Color(1.0f, 0.0f, 1.0f, (float) strength);
		}

		Point adjustedCenterPoint = myFaceModel.getObsCenterAdjustedScreenPoint(obs, true);

		g.setColor(hypoColor);
		if (vp.inBounds(adjustedCenterPoint)) {
			Rectangle boundRect = ((FaceObservation) obs).getBoundRect();
			int width = boundRect.width;
			int height = boundRect.height;
			int adjX = adjustedCenterPoint.x - width / 2;
			int adjY = adjustedCenterPoint.y - height / 2;
			g.fill3DRect(adjX, adjY, width, height, true);
			int textX = adjX + 5;
			int textY = adjY+ 10;
			g.setColor(Color.YELLOW);
			g.drawString("H-" + h.getHypothesisNumber(), textX, textY);

			String tval = ((FaceHypothesis) h).getTagValue();

			if (tval != null) {
				textY += 15;
				g.drawString("TV=" + tval, textX, textY);
			}
			EgocentricDirection obsDir = obs.getCenterDirection();
			if (obsDir != null) {
				textY += 15;
				g.drawString("az=" + obsDir.getAzimuth().getDegreesText(), textX, textY);
				textY += 15;
				g.drawString("el=" + obsDir.getElevation().getDegreesText(), textX, textY);
			}
			double diamDeg = obs.getDiameterDeg(vp);
			textY += 15;
			g.drawString("diam=" + diamDeg, textX, textY);
			textY += 15;
			String attStatString = (attentionStatus == null) ? "UNRECOGNIZED" : attentionStatus.toString();
			g.drawString("stat=" + attStatString, textX, textY);
		}
	}
	private void drawCameraDirectionInfo(SightPort vp, Graphics g) {
		EgocentricDirection cameraDir = myFaceModel.getCameraCenterDirection(false); 
		if (cameraDir != null) {
			g.setColor(Color.RED);
			Point cameraCenterOnScreen = vp.getCameraCenterPixel();
			int centerX = cameraCenterOnScreen.x;
			int centerY = cameraCenterOnScreen.y;
			g.drawString("c-az=" + cameraDir.getAzimuth().getDegreesText(), centerX - 15, 15);
			g.drawString("c-el=" + cameraDir.getElevation().getDegreesText(), 5, centerY - 6);
			g.drawLine(centerX, 0, centerX, vp.getHeightPixels());
			g.drawLine(0, centerY, vp.getWidthPixels(), centerY);
		}
	}
}
