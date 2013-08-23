/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author matt
 */
public class FrameFaceInformation {
	public enum ShirtColor{Black,White,Grey,Red,Orange,Yellow,Green,Aqua,Blue,Purple,Pink}
	
	public static boolean captureStart;
	public static int	  captureStartId;
	public static List<Integer> capturedIds;

	private int myFaceImageId;
	private int myFaceMatchId;
	private double myEigenDistance;
	private Rectangle myRectangle;
	private List<Integer> myShirtColros;

	public FrameFaceInformation(int[] data){
		myRectangle = new Rectangle(data[0], data[1], data[2], data[3]);
		myShirtColros = new ArrayList<Integer>();
		for(int i=0; i<11; i++){
			myShirtColros.add(data[4+i]);
		}
		myFaceMatchId = data[15];
		myFaceImageId = data[16];
		myEigenDistance = (double)data[17]/100.0;
	}

	public double getMatchDistance() {
		return myEigenDistance;
	}
	public int getFaceMatchId() {
		return myFaceMatchId;
	}

	public int getFaceImageId() {
		return myFaceImageId;
	}

	public Rectangle getRectangle() {
		return myRectangle;
	}

	public List<Integer> getShirtColros() {
		return myShirtColros;
	}
}
