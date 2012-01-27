/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.model.io;

import	java.io.File;
import	java.io.FileFilter;

import java.util.List;
import java.util.ArrayList;

import java.util.logging.Logger;

import org.friendularity.nwrap.joint.JointAnimationPacket;
import java.util.logging.Level;

import org.cogchar.animoid.oldconfig.IntMatrixFuncs;
import org.cogchar.api.animoid.config.bonus.ServoChannelConfig;
import org.cogchar.platform.util.BoundsAssertions;


/**
 *
 * If we export the first 32 channels of a VSA file,  in "sparse" format, WITH initial positions, 
 * the data looks like this:
 * 
83, 130, 170, 127, 68, 115, 130, 73, 129, 131, 115, 202, 125, 182, 135, 110, 125, 103, 70, 56, 125, 170, 122, 119, 125, 114, 103, 130, 127, 97, 87, 114, 
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 

 * 
 * 
 * @author humankind
 */
public class VSA_Reader {
	private static Logger	theLogger = Logger.getLogger(VSA_Reader.class.getName());
		
	static JointAnimationPacket buildJointAnimationPacket(String animationName, 
				int[][] vsaAnimRows, ServoChannelConfig[] servoConfArray, boolean ignoreInvalidChannels) 
				throws Throwable {
		boolean adjusted = false;
		int frameCount = vsaAnimRows.length;
		JointAnimationPacket jap = new JointAnimationPacket(frameCount);
		jap.mapAndAllocate();
		jap.animationCommand.set(JointAnimationPacket.JointAnimationCommand.ADD_NAMED_TO_LIBRARY);
		jap.animationName.set(animationName);
		for (int f=0; f < frameCount; f++) {
			int[] animRow = vsaAnimRows[f];
			for (int j=0; j < animRow.length; j++) {
				int animVal = animRow[j];
				// TODO: This will construct a lot of unused strings (unless hotspot is smart)...
				BoundsAssertions.checkInclusiveBounds(animVal, -1, 250, 
							"Animation move position at row=" + f + " col=" + j);
				// VSA indicates no-op in a sparse matrix with "-1"
				if (animVal == -1) {
					continue;
				}
				// We have a move value between 0 & 250.  Let's look in the servo map.
				ServoChannelConfig	servoConf = servoConfArray[j];
				if (servoConf == null) {
					if (ignoreInvalidChannels) {
						continue;
					} else {
						throw new Exception("Got unexpected animVal " + animVal 
							+ " for unconfigured physical servo channel " + j + " at row=" + f);
					}
				}
				/*
				if ((animVal < servoConf.minPos) || (animVal > servoConf.maxPos)) {
					throw new Exception ("Got animVal " + animVal + " on physical channel " + j + 
						" at row " + f + ", which is outside of the allowed range: [" 
							+ servoConf.minPos + "," + servoConf.maxPos + "]");
				}
				 */ 
				if (animVal < servoConf.minPos) {
					theLogger.finer("Value " + animVal + " on channel " + j + " at frame " + f 
								+ " is below minimum " + servoConf.minPos 
								+ " -- adjusting to minimum value!");
					animVal = servoConf.minPos;
					adjusted = true;
				}
				if (animVal > servoConf.maxPos) {
					theLogger.finer("Value " + animVal + " on channel " + j + " at frame " + f 
								+ " is above maximum " + servoConf.maxPos 
								+ " -- adjusting to maximum value!");
					animVal = servoConf.maxPos;
					adjusted = true;
				}

				// Just live with the silly two-sided coordinate system, for the moment:
				double mjValue = servoConf.convertAbsServoIntToLopsidedFloat(animVal);
				jap.frames[f].addAbsoluteImmediateMove(servoConf.logicalChannel, mjValue);
			}	
		}
		if (adjusted) {
			theLogger.warning("Animation " + animationName + " required adjustments!");
		}
		return jap;
	}
		
	public static File[] getAnimationFilesInDirectory(String directoryPath) throws Throwable {
		File	directory = new File(directoryPath);
		if (!directory.isDirectory()) {
			throw new Exception ("Path[" + directoryPath + "] does not refer to an accessible directory");
		}
		File[] files = directory.listFiles(new FileFilter() {
			public boolean accept(File f) {
				String name = f.getName();
				if (f.isFile() && f.getName().endsWith(".csv")) {
					return true;
				} else {
					theLogger.finer("Skipping non-VSA-animation file: " + name);
					return false;
				}
			} 
		});
		return files;
	}
	/* 
	 *  Returns a list of relative filenames within the directory.
	 */
	
	public static List<JointAnimationPacket>  readAnimationsInDirectory(String dirPath,
				ServoChannelConfig[] servoConfArray, boolean ignoreInvalidChannels) throws Throwable {
		List<JointAnimationPacket>	animList = new ArrayList<JointAnimationPacket>();
		File[]		files = getAnimationFilesInDirectory(dirPath);
		for (int i=0; i < files.length; i++) {
			JointAnimationPacket jap = readAnimationFile(files[i], servoConfArray, ignoreInvalidChannels);
			if (jap != null) {
				animList.add(jap);
			}
		}
		return animList;
	}
	public static JointAnimationPacket readAnimationFile(File f, ServoChannelConfig[] servoConfArray,
				boolean ignoreInvalidChannels) {
		JointAnimationPacket jap = null;
		String filename = f.getName();
		String fullPath = f.getAbsolutePath();
		try {
			theLogger.fine("Loading animation file: " + filename);
			int [][] animationRows = IntMatrixFuncs.readAndVerifyMatrixFile(fullPath, 32);
			// Strip ".csv" suffix from filename
			String animationName = filename.substring(0, filename.length() - 4);
			jap = buildJointAnimationPacket(animationName,
					animationRows, servoConfArray, ignoreInvalidChannels);
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "Error loading animation: " + fullPath, t);
		}
		return jap;
	}
	public static void main(String args[]) {
		String testDistroDir = "C:/_hanson/_deploy/distro_09_msi/";
		String testServoConfigFilename =  testDistroDir + "conf/zeno/servo_2008/model_01/zm01_sc_20080606_match_vsa.txt";
		
		String testAnimDirectory = testDistroDir + "conf/zeno/vsa_export/";

		String testAnimFilename  =  testAnimDirectory + "all_servos_test_20080518_all32_sparse_withoutInit.csv";
			
		try {
			// We won't get 32 config rows if some are commented out in the config file.
			int [][] servoConfigRows = IntMatrixFuncs.readAndVerifyMatrixFile(testServoConfigFilename, 6);
			ServoChannelConfig[] servoConfArray = 
					ServoChannelConfig.buildValidServoConfigArray(servoConfigRows, 32);
			int [][] animationRows = IntMatrixFuncs.readAndVerifyMatrixFile(testAnimFilename, 32);
			JointAnimationPacket jap = buildJointAnimationPacket("theMacarena", 
					animationRows, servoConfArray, false);
			
			List<JointAnimationPacket> animList = readAnimationsInDirectory(testAnimDirectory,
						servoConfArray, false);
			
			theLogger.info("Loaded " + animList.size() + " animations");
			
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}
}
