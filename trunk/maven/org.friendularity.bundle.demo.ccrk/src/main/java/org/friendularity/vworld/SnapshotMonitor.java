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
package org.friendularity.vworld;

import org.cogchar.bind.midi.in.TempMidiBridge;
import com.jme3.asset.AssetManager;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.Rectangle;
import com.jme3.light.DirectionalLight;
import com.jme3.material.Material;
import com.jme3.material.RenderState;
import com.jme3.material.RenderState.FaceCullMode;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Matrix3f;
import com.jme3.math.Vector3f;
import com.jme3.math.Quaternion;
import com.jme3.renderer.ViewPort;
import com.jme3.renderer.queue.RenderQueue;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.control.BillboardControl;
import com.jme3.scene.shape.Dome;
import com.jme3.scene.shape.Quad;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.FreeIdent;
import org.cogchar.render.opengl.scene.TextMgr;
import org.cogchar.render.sys.context.CogcharRenderContext;
import org.cogchar.render.sys.context.CoreFeatureAdapter;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.cogchar.render.sys.task.Queuer;

import java.util.List;
import java.util.ArrayList;

import org.friendularity.struct.RingBuf;
import org.friendularity.struct.Factory;

import org.cogchar.render.trial.TrialContent;
// import org.cogchar.render.trial.TrialCameras;
// import org.cogchar.render.trial.TrialCameras.CamCoord;
import org.cogchar.render.trial.MathUtils;

import com.jme3.texture.Texture2D;
import org.cogchar.bind.midi.in.ParamValueListener;

import org.cogchar.bind.midi.in.CCParamRouter;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class SnapshotMonitor extends TrialContent implements ParamValueListener {

	private JVisionTextureMapper myJVTM;

	enum Mode {
		FIXED_POS,
		FOLLOW_CURSOR
	}
	
	private Mode		myMode = Mode.FIXED_POS;
	
	public static class PanelItem {

		Texture2D myTexture;
		Material myMat;
		Geometry myGeom;
		float myAlphaOpacity = 0.5f;
		ColorRGBA myColor = new ColorRGBA(1.0f, 1.0f, 1.0f, myAlphaOpacity);
		int		myAbsIndex;
	}

	public static class ItemFactory extends Factory<PanelItem> {

		@Override public PanelItem[] makeArray(int size) {
			return new PanelItem[size];
		}

		@Override public PanelItem makeOne() {
			return new PanelItem();
		}

		@Override public void shallowCopyContents(PanelItem source, PanelItem target) {
			target.myTexture = source.myTexture;
		}
	}
	private float myCursorAz = 0.0f, myCursorEl, myCursorTwist, myCursorDist = 15.0f;
	private Vector3f myBaseVec;
	private RingBuf<PanelItem> myIRB;
	private Node mySnapRootNode;
	private	Geometry myCursorGeom;
	// private TempMidiBridge	myTMB;
	public void setup_onRendThrd(RenderRegistryClient rrc, Node parentNode) {
		mySnapRootNode = new Node("snapshot_monitor_root");

		myBaseVec = new Vector3f(5.0f, 10.0f, 5.0f);

		Material baseMat = makeAlphaBlendedUnshadedMaterial(rrc, 1.0f, 1.0f, 1.0f, 1.0f);

		int panelCount = 10;

		// On a mat, we could choose to set the cull mode, but that can also be done 
		// on shapes - as below.
		ItemFactory anItemFactory = new ItemFactory();

		int panelWidth = 32, panelHeight = 24;
		

		myCursorGeom = new Geometry("snapshot_cursor", new Quad(panelWidth, panelHeight));
		Material cursorMat = baseMat.clone();
		ColorRGBA cursorColor = new ColorRGBA(0.8f, 0.2f, 0.8f, 0.5f);
		cursorMat.setColor("Color", cursorColor);
		myCursorGeom.setMaterial(cursorMat);
		mySnapRootNode.attachChild(myCursorGeom);
		
		
		myIRB = new RingBuf<PanelItem>(panelCount, anItemFactory);
		for (int panelIndex = 0; panelIndex < panelCount; panelIndex++) {
			PanelItem panelItem = myIRB.getCurrent();
			panelItem.myAbsIndex = panelIndex;
			myIRB.advance();

			
			Geometry qg = new Geometry("snapshot_pvq_" + panelIndex, new Quad(panelWidth, panelHeight));
			Material panelMat = baseMat.clone();
			qg.setMaterial(panelMat);
			configureRenderingForSpatial(qg);  // Sets the rendering bucket and cull mode

			Vector3f offsetV = calculateDefaultPosition(panelIndex);
			qg.setLocalTranslation(offsetV);
			mySnapRootNode.attachChild(qg);

			panelItem.myMat = panelMat;
			panelItem.myGeom = qg;

		}
		mySnapRootNode.setLocalTranslation(myBaseVec);
		parentNode.attachChild(mySnapRootNode);
	}
	private Vector3f calculateDefaultPosition(int panelIndex) { 
		float d = panelIndex * 25.0f;
		Vector3f offsetV = new Vector3f(-0.8f * d, -20.0f + 0.5f * d, -3.0f - 1.0f * d);
		return offsetV;
	}
	public void setJVisionTextureMapper(JVisionTextureMapper jvtm) {
		myJVTM = jvtm;
	}

	public void setMode(Mode m) {
		myMode = m;
	}
	public void update_onRendThrd(float tpf) {
		updateCursorPos_onRendThread(tpf);
		PanelItem panelItem = myIRB.getCurrent();
		if (myJVTM != null) {
			// Will be null if there is no new frame since last take() (which currently happens in MagicVisionBox)
			Texture2D latestTexture = myJVTM.peekLatestTexture();
			if (latestTexture != null) {
				panelItem.myTexture = latestTexture;
				panelItem.myMat.setTexture("ColorMap", panelItem.myTexture);
				panelItem.myGeom.setMaterial(panelItem.myMat);
				if (myMode == Mode.FOLLOW_CURSOR) {
					Vector3f cursPos = myCursorGeom.getLocalTranslation();
					Quaternion cursRot = myCursorGeom.getLocalRotation();
					panelItem.myGeom.setLocalTranslation(cursPos);
					panelItem.myGeom.setLocalRotation(cursRot);
				} else {
					Vector3f offsetV = calculateDefaultPosition(panelItem.myAbsIndex);
					panelItem.myGeom.setLocalTranslation(offsetV);
				}
				adjustOpacities_onRendThread();
				myIRB.advance();
			} else {
				// DO NOT ADVANCE!
			}
		} else {
			
			float colorPhase = (System.currentTimeMillis() % 3000) / 3000.0f;
			ColorRGBA color = new ColorRGBA(colorPhase, 0.3f, 0.0f, 0.5f);
			panelItem.myMat.setColor("Color", color);
			panelItem.myGeom.setMaterial(panelItem.myMat);
			adjustOpacities_onRendThread();
			myIRB.advance();
		}
	}

	private void adjustOpacities_onRendThread() {
		int panelCount = myIRB.mySize();
		for (int howFarBack = 0; howFarBack < panelCount; howFarBack++) {
			PanelItem prevItem = myIRB.getPrevious(howFarBack);
			prevItem.myAlphaOpacity = (panelCount - howFarBack )/ (1.0f * panelCount);
			float oldRed = prevItem.myColor.getRed();
			float oldGreen = prevItem.myColor.getGreen();
			float oldBlue = prevItem.myColor.getBlue();
			prevItem.myColor.set(oldRed, oldGreen, oldBlue, prevItem.myAlphaOpacity);
			prevItem.myMat.setColor("Color", prevItem.myColor);
			prevItem.myGeom.setMaterial(prevItem.myMat);
		}
	}
	private void updateCursorPos_onRendThread(float tpf) { 
		float eulerAngles[] = {myCursorEl, myCursorAz, myCursorTwist};
		Quaternion quat = new Quaternion(eulerAngles);
		Vector3f base = Vector3f.UNIT_Z;
		Vector3f rotated = quat.mult(base);
		Vector3f cpos = rotated.mult(myCursorDist);
		myCursorGeom.setLocalTranslation(cpos);
		myCursorGeom.setLocalRotation(quat);
	}
	enum Coord {
		AZIMUTH, ELEVATION, TWIST, DEPTH, MODE
	}
	@Override public void attachMidiCCs(CCParamRouter ccpr) { 
		ccpr.putControlChangeParamBinding(27, Coord.AZIMUTH.name(), this); 
		ccpr.putControlChangeParamBinding(28, Coord.ELEVATION.name(), this); 
		ccpr.putControlChangeParamBinding(26, Coord.TWIST.name(), this);
		
		// Experiment:  assign CC #40 to a crossfader, e.g. on a Nocturn
		ccpr.putControlChangeParamBinding(40, Coord.DEPTH.name(), this); 
		// Also put it on a regular knob
		ccpr.putControlChangeParamBinding(25, Coord.DEPTH.name(), this); 	
		ccpr.putControlChangeParamBinding(58, Coord.MODE.name(), this); 	
	}		
	@Override public void setNormalizedNumericParam(String paramName, float normZeroToOne) {
		Coord ccoord = Coord.valueOf(paramName);
		float halfPi = (float) (0.5 * Math.PI);
		switch (ccoord) {
			case AZIMUTH:
				myCursorAz = MathUtils.getFloatValInRange(-1 * halfPi,  halfPi, normZeroToOne);
			break;
			case ELEVATION:
				myCursorEl = MathUtils.getFloatValInRange(-1 * halfPi,  halfPi, normZeroToOne);				
			break;
			case TWIST:
				myCursorTwist = MathUtils.getFloatValInRange(-1 * halfPi,  halfPi, normZeroToOne);				
			break;				
			case DEPTH:
				myCursorDist = MathUtils.getFloatValInRange(-50.0f,  50.0f, normZeroToOne);				
			break;
			case MODE:
				if (normZeroToOne > 0.5) {
					myMode = Mode.FOLLOW_CURSOR;
				} else {
					myMode = Mode.FIXED_POS;
				}
			break;
			default:
				getLogger().warn("Unknown numeric-param channel name: {} resolved to: {}", paramName, ccoord);
		}
		// getLogger().info("Params az={}, el={}, tw={}, de={}", myCursorAz, myCursorEl, myCursorTwist, myCursorDist);
	}
}
