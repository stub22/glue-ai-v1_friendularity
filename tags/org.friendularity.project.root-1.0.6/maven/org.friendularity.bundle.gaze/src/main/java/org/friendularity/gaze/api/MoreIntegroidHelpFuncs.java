/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.gaze.api;

import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.api.animoid.gaze.IGazeTarget;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class MoreIntegroidHelpFuncs {
	public static AnimoidGazeFacade getAGF(IntegroidFacade igf) { 
		return (AnimoidGazeFacade) igf.getAnimoidFacade();
	}
	public static void setGazeStrategy(IntegroidFacade igf, String name){
		getAGF(igf).suggestGazeStrategyName(name);
	}
	public static void setHoldAndRecenterStrategy(IntegroidFacade igf, String name){
		getAGF(igf).suggestHoldStrategyName(name);
	}

	public static void suggestGazeTarget(IntegroidFacade igf, IGazeTarget igt){
		getAGF(igf).suggestAttentionTarget(igt);
	}	
}
