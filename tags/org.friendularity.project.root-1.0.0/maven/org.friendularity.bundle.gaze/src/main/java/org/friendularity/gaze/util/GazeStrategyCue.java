/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.gaze.util;


import java.util.ArrayList;
import java.util.List;

import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
import org.friendularity.gaze.api.GazeJoint;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.friendularity.gaze.api.GlanceStrategy;
import org.cogchar.zzz.platform.stub.CueStub;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class GazeStrategyCue extends CueStub {
	private static Logger	theLogger = LoggerFactory.getLogger(GazeStrategyCue.class.getName());

	public enum MotionStyle {
		NONE,
		JUMPY,
		RAMPY,
		HOLD_AND_RECENTER
	}
	private		String				name;

	private		MotionStyle			motionStyle;
	private		List<GazeJointStrategy>	myJointLinks;
	// private		Integer				centerPixHoriz, centerPixVert; // What pixel should be treated as horiz/vert "center"
	private		Double				slackHorizDeg, 	slackVertDeg;

	private		Integer				refreshPeriodFrames;

	public		Double				brakeSlope, brakePower;
	public		Double				flatJumpSize, distanceJumpRatio;
	
	private		GlanceStrategy		glanceStrategy;

	public 		String				helperStrategyName;

	private		GazeStrategyCue		myHelperStrategy;

	private	transient	List<GazeJointStrategy>		myHorizLinks, myVertLinks;
	// private	transient	Point					myCenterPoint;
	
	private	transient	AnimoidConfig			myParentConfig;
	
	// private	transient	Float					computedPixRateHoriz;
	// private	transient	Float					computedPixRateVert;
	public void completeInit(AnimoidConfig animConf) {
		myParentConfig = animConf;

		if (motionStyle == null) {
			motionStyle = MotionStyle.JUMPY;
		}
		if (brakeSlope == null) {
			brakeSlope = 0.0;
		}
		if (brakePower == null) {
			brakePower = 1.0;
		}
		if (flatJumpSize == null) {
			flatJumpSize = 0.0;
		}
		if (distanceJumpRatio == null) {
			distanceJumpRatio = 1.0;
		}
		if (refreshPeriodFrames == null) {
			refreshPeriodFrames = 1;
		}
		/*
		if (centerPixHoriz == null) {
			centerPixHoriz = 160;
		}
		if (centerPixVert == null) {
			centerPixVert = 120;
		}
		 */
		// myCenterPoint = new Point(centerPixHoriz, centerPixVert);
		myHorizLinks = new ArrayList<GazeJointStrategy>();
		myVertLinks = new ArrayList<GazeJointStrategy>();
		if (myJointLinks == null) {
			myJointLinks = new ArrayList<GazeJointStrategy>();
		}
		for (GazeJointStrategy gjl : myJointLinks) {
			gjl.completeInit(animConf);
			GazeJoint gj = gjl.getGazeJoint();
			if (gj.isHorizontal()) {
				myHorizLinks.add(gjl);
			} else {
				myVertLinks.add(gjl);
			}
		}		
		if (glanceStrategy != null) {
			glanceStrategy.completeInit();
		}
		if (helperStrategyName != null) {
			if (motionStyle != MotionStyle.HOLD_AND_RECENTER) {
				throw new RuntimeException(
					"Got unexpected helperStrategyName[" + helperStrategyName
					+ "] on non-hold-and-recenter parent: " + this);
			}
			// myHelperStrategy = animConf.getNamedGazeStrategy(helperStrategyName);
		}
	}

	public GazeJointStrategy getLinkForLogicalID(int id) {
		for(GazeJointStrategy gsc : myJointLinks){
			if(gsc.getLogicalJointID() == id){
				return gsc;
			}
		}
		return null;
	}
	public List<GazeJointStrategy> getHorizLinks() {
		return myHorizLinks;
	}
	public List<GazeJointStrategy> getVertLinks() {
		return myVertLinks;
	}
	
	public String getName() {
    	return name;
    }

	public void setName(String name) {
    	this.name = name;
    }


	public List<GazeJointStrategy> getJointLinks() {
    	return myJointLinks;
    }
/*
	public Integer getCenterPixHoriz() {
    	return centerPixHoriz;
    }
	public void setCenterPixHoriz(Integer centerPixHoriz) {
    	this.centerPixHoriz = centerPixHoriz;
    }
	public Integer getCenterPixVert() {
    	return centerPixVert;
    }
	public void setCenterPixVert(Integer centerPixVert) {
    	this.centerPixVert = centerPixVert;
    }
 */
	public Double getSlackHorizDeg() {
    	return slackHorizDeg;
    }
	public void setSlackHorizDeg(Double slackHorizDeg) {
    	this.slackHorizDeg = slackHorizDeg;
    }
	public Double getSlackVertDeg() {
    	return slackVertDeg;
    }
	public void setSlackVertDeg(Double slackVertDeg) {
    	this.slackVertDeg = slackVertDeg;
    }
	public String toString() {
		return "\nGazeStrategyCue[name=" + name
				+ ", motionStyle=" + motionStyle
		//		+ ", centerPoint=" + myCenterPoint
				+ ", jointLinks=" + myJointLinks 
				+ ", refreshPeriodFrames=" + refreshPeriodFrames
				+ ", glanceStrategy=" + glanceStrategy 
				+ ", helperStrategyName=" + helperStrategyName
				+ "]";
	}

	public GlanceStrategy getGlanceStrategy() {
		return glanceStrategy;
	}
	public GazeStrategyCue getHelperStrategy() {
		return myHelperStrategy;
	}
	/*
	public Point getCenterPoint() {
		return myCenterPoint;
	}
	 */
	public Integer getRefreshPeriodFrames() {
    	return refreshPeriodFrames;
    }
	public void setRefreshPeriodFrames(Integer refreshPeriodFrames) {
    	this.refreshPeriodFrames = refreshPeriodFrames;
    }
	public AnimoidConfig getParentConfig() {
		return myParentConfig;
	}
	public MotionStyle getMotionStyle() {
		return motionStyle;
	}

	public Double getBrakePower() {
		return brakePower;
	}

	public void setBrakePower(Double brakePower) {
		this.brakePower = brakePower;
	}

	public Double getBrakeSlope() {
		return brakeSlope;
	}

	public void setBrakeSlope(Double brakeSlope) {
		this.brakeSlope = brakeSlope;
	}

	public Double getDistanceJumpRatio() {
		return distanceJumpRatio;
	}

	public void setDistanceJumpRatio(Double distanceJumpRatio) {
		this.distanceJumpRatio = distanceJumpRatio;
	}

	public Double getFlatJumpSize() {
		return flatJumpSize;
	}

	public void setFlatJumpSize(Double flatJumpSize) {
		this.flatJumpSize = flatJumpSize;
	}

	public static boolean isActiveStrategy(GazeStrategyCue candidate) {
		return ((candidate != null) && (candidate.getMotionStyle() != MotionStyle.NONE));
	}
	
}
