/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.tmpgood.tgbit;

import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Cylinder;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.cogchar.name.goody.GoodyNames;
import org.cogchar.render.app.entity.GoodyActionExtractor;
import org.cogchar.render.app.entity.VWorldEntity;
import org.cogchar.render.goody.basic.BasicGoodyCtx;
import org.cogchar.render.goody.basic.BasicGoodyEntity;
import org.cogchar.render.goody.basic.CompositeMeshBuilder;
import org.cogchar.render.goody.basic.CompositeMeshBuilder.MeshComponent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A class to implement the Robosteps Tic-tac-toe grid objects and functionality
 *
 * @author Ryan Biggs <rbiggs@hansonrobokind.com>
 */


public class TG_TicTacGrid extends BasicGoodyEntity {
	
	private static final ColorRGBA DEFAULT_GRID_COLOR = ColorRGBA.Blue;
	private static final float SIZE_MULTIPLIER = 9f;
	private static final float[] ROTATE_UPRIGHT = {(float)(Math.PI/2), 0f, 0f};
	public static final Ident CLEAR_IDENT = GoodyNames.makeID("clearMarks");
	
	private Map<Ident, TG_TicTacMark> markMap = new HashMap<Ident, TG_TicTacMark>();
	
	public TG_TicTacGrid(BasicGoodyCtx bgc, Ident boxUri, Vector3f initialPosition,
						 Quaternion initialRotation, ColorRGBA color, Vector3f size) {
		super(bgc, boxUri);
		setPositionRotationAndScale(initialPosition, initialRotation, size, QueueingStyle.QUEUE_AND_RETURN);
		Mesh gridMesh = makeCustomGridMesh();
		if (color == null) {
			color = DEFAULT_GRID_COLOR;
		}
		addGeometry(gridMesh, color, new Quaternion(ROTATE_UPRIGHT));
	}
	
	private Mesh makeCustomGridMesh() {
		Mesh gridLeg = new Cylinder(20, 20, 1f/5f, SIZE_MULTIPLIER, true);
		float offsetDistance = SIZE_MULTIPLIER/6f;
		List<MeshComponent> meshComponents = new ArrayList<MeshComponent>();
		meshComponents.add(new MeshComponent(gridLeg, new Vector3f(offsetDistance, 0f, 0f)));
		meshComponents.add(new MeshComponent(gridLeg, new Vector3f(-offsetDistance, 0f, 0f)));
		Quaternion rotate90DegAroundY = new Quaternion();
		rotate90DegAroundY.fromAngleAxis((float)Math.PI/2, new Vector3f(0f,1f,0f));
		meshComponents.add(new MeshComponent(gridLeg, rotate90DegAroundY, new Vector3f(0f, 0f, offsetDistance)));
		meshComponents.add(new MeshComponent(gridLeg, rotate90DegAroundY, new Vector3f(0f, 0f, -offsetDistance)));
		CompositeMeshBuilder builder = new CompositeMeshBuilder();
		return builder.makeCompositeMesh(meshComponents);
	}
	
	public void addMarkAt(int xPos, int yPos, boolean isPlayerO) {
		Ident markUri = createMarkIdent(xPos, yPos);
		if ((xPos < 1) || (xPos > 3) || (yPos < 1) || (yPos > 3)) {
			getLogger().error("Can't add TicTacMark to grid; position of ({}, {}) is invalid", xPos, yPos);
		} else if (markMap.containsKey(markUri)) {
			getLogger().warn("Can't add TicTacMark to grid; there is already a mark at position ({}, {})", xPos, yPos);
		} else {
			Vector3f markPosition = getWorldPositionForMark(xPos, yPos);
			Quaternion rotation = getRotation();
			Vector3f scale = getScale();
			BasicGoodyCtx bgc = getGoodyCtx();
			TG_TicTacMark markGoody =
					new TG_TicTacMark(bgc, markUri, markPosition, rotation, scale, isPlayerO);
			// getTheGoodySpace().addGoody(markGoody);
			bgc.getVWER().addGoody(markGoody);
			Node parentNode = getParentNode();
			markGoody.attachToVirtualWorldNode(parentNode, VWorldEntity.QueueingStyle.QUEUE_AND_RETURN);
			markMap.put(markUri, markGoody);
		}
	}
	
	public void removeMark(int xPos, int yPos) {
		Ident markIdent = createMarkIdent(xPos, yPos);
		BasicGoodyEntity markToRemove = markMap.get(markIdent);
		if (markToRemove != null) {
			getGoodyCtx().getVWER().removeGoody(markToRemove);
			markMap.remove(markIdent);
		} else {
			getLogger().warn("No TicTacMark to remove at location ({}, {})", xPos, yPos);
		}
	}
	
	public void clearMarks() {
		for (BasicGoodyEntity mark : markMap.values()) {
			getGoodyCtx().getVWER().removeGoody(mark);
		}
		markMap.clear();
	}
	
	private Ident createMarkIdent(int xPos, int yPos) {
		String uriString = getUri().getAbsUriString();
		uriString += "Mark" + xPos + yPos;
		return new FreeIdent(uriString);
	}
	
	private int[] getGridPosition(Ident markIdent) {
		String markName = markIdent.getLocalName();
		// Mark x and y are part of local name per createMarkIdent:
		int xPos = Integer.valueOf(String.valueOf(markName.charAt(markName.length() - 2)));
		int yPos = Integer.valueOf(String.valueOf(markName.charAt(markName.length() - 1)));
		return new int[]{xPos, yPos};
	}
	
	private Vector3f getWorldPositionForMark(int xPos, int yPos) {
		Quaternion rotation = getRotation();
		Vector3f scale = getScale();
		Vector3f position = getPosition();
		float markOffsetX = SIZE_MULTIPLIER*scale.getX()/3f;
		float markOffsetY = SIZE_MULTIPLIER*scale.getY()/3f;
		Vector3f relativeMarkPosition = new Vector3f(markOffsetX*(xPos-2), -markOffsetY*(yPos-2), 0);
		// Now rotate positions according to myRotation
		relativeMarkPosition = rotation.mult(relativeMarkPosition);
		return position.add(relativeMarkPosition); 
	}
	
	private Vector3f getWorldPositionForMark(TG_TicTacMark mark) {
		int[] gridPosition = getGridPosition(mark.getUri());
		return getWorldPositionForMark(gridPosition[0], gridPosition[1]);
	}

	// Protected so it can be overridden
//	protected VWorldEntityActionConsumer getTheGoodySpace() {
//		return GoodyFactory.getTheFactory().getActionConsumer();
//	}
	
	// On detach, we also want to remove all marks
	@Override public void detachFromVirtualWorldNode(QueueingStyle qStyle) {
		clearMarks();
		super.detachFromVirtualWorldNode(qStyle);
	}
	
	@Override public void setPositionAndRotation(Vector3f newPosition, Quaternion newRotation, QueueingStyle qStyle) {
		Vector3f scale = getScale();
		setPositionRotationAndScale(newPosition, newRotation, scale, qStyle);
	}
	
	@Override public void setUniformScaleFactor(Float newScale, QueueingStyle qStyle) {
		setVectorScale(new Vector3f(newScale, newScale, newScale), qStyle);
	}
	@Override public void setVectorScale(Vector3f newScale, QueueingStyle qStyle) {
		Quaternion rotation = getRotation();
		Vector3f position = getPosition();		
		setPositionRotationAndScale(position, rotation, newScale, qStyle);
	}
	
	 final public void setPositionRotationAndScale(Vector3f newPosition, Quaternion newRotation, Vector3f newScale, QueueingStyle qStyle) {
		super.setPositionAndRotation(newPosition, newRotation, qStyle);
		super.setVectorScale(newScale, qStyle);
		for (TG_TicTacMark markGoody : markMap.values()) {
			markGoody.setVectorScale(newScale, qStyle);
			markGoody.setPositionAndRotation(getWorldPositionForMark(markGoody), newRotation, qStyle);
		}
	}
	
	@Override
	protected void moveViaAnimation(Vector3f newPosition, Quaternion newOrientation, Vector3f newScale, float duration) {
		getLogger().warn("MOVE not yet supported for TicTacGrid, coming soon...");
	}
	
	@Override public void applyAction(GoodyActionExtractor ga, QueueingStyle qStyle) {
		super.applyAction(ga, qStyle);
		switch (ga.getKind()) {
			case SET : {
				Boolean clearMarksFlag = ga.getSpecialBoolean(CLEAR_IDENT);
				Boolean stateFlag = ga.getSpecialBoolean(GoodyNames.USE_O);
				if ((clearMarksFlag != null) && clearMarksFlag.booleanValue()) {
					try {
						if (Boolean.valueOf(clearMarksFlag)) {
							clearMarks();
						}
					} catch (Exception e) {	
					}
				} else if (stateFlag != null) {
					try {
						int xCoord = ga.getSpecialInteger(GoodyNames.COORDINATE_X);
						int yCoord = ga.getSpecialInteger(GoodyNames.COORDINATE_Y);
						addMarkAt(xCoord, yCoord, stateFlag);
					} catch (Exception e) { // May not need try/catch after BasicTypedValueMap implementation is complete
						getLogger().error("Error interpreting parameters for adding mark to TicTacGrid", e);
					}
				}
				break;
			}
		}
	}
}
