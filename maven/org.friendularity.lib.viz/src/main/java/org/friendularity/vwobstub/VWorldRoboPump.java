/*
 *  Copyright 2014 by The Cogchar Project (www.cogchar.org).
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
package org.friendularity.vwobstub;

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.cogchar.api.skeleton.config.BoneProjectionRange;
import org.cogchar.bind.mio.robot.model.ModelJoint;
import org.cogchar.bind.mio.robot.model.ModelRobot;
// import org.cogchar.bundle.app.vworld.startup.ModelToFigureStateMappingFuncs;
import org.cogchar.render.model.bony.FigureState;
import org.cogchar.render.model.humanoid.HumanoidFigure;

import java.util.List;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class VWorldRoboPump extends BasicDebugger {

	Ident			myPumpID;
	ModelRobot myModelRobot;
	HumanoidFigure	myHumaFig;

	public VWorldRoboPump(Ident pumpID, ModelRobot mr, HumanoidFigure hf) {
		myPumpID = pumpID;
		myModelRobot = mr;
		myHumaFig = hf;
	}
	public boolean completeSetup() { 
		setupFigureState();
		registerMoveListener();
		return true;
	}
	class PumpML implements ModelRobot.MoveListener {
		@Override public void notifyBonyRobotMoved(ModelRobot br) {
			// TODO:  Check on the status of myHumaFig
			if ((br != null) & (myHumaFig != null)) {
				ModelToFigureStateMappingFuncs.propagateState(br, myHumaFig);
			}
		}
	}

	protected void registerMoveListener() {
		PumpML ourPump = new PumpML();
		myModelRobot.registerMoveListener(ourPump);
	}

	protected void setupFigureState() {
		// It is optional to create this state object if there is no humanoid figure to animate.
		// could be used for some other programming purpose.
		FigureState fs = buildFigureState(myModelRobot);
		getLogger().info("FigureState={}", fs);
		if (myHumaFig != null) {
			myHumaFig.setFigureState(fs);
		} else {
			getLogger().warn("figState aborting due to missing HumanoidFigure, for pumpID={}", myPumpID);
		}
	}
    private FigureState buildFigureState(ModelRobot br) {

        FigureState fs = new FigureState();
        List<ModelJoint> allJoints = br.getJointList();
        for (ModelJoint mJoint : allJoints) {
            for (BoneProjectionRange bpr : mJoint.getBoneRotationRanges()) {
                String boneName = bpr.getBoneName();
                // BoneState is returned, but ignored here.
                fs.obtainBoneState(boneName);
            }
        }
        return fs;
    }	
/*
	public void connect() throws Exception {
		getLogger().info("charID={}, ModelRobot={}, robotID={}", charID, mr, mr.getRobotId());
		final ModelRobot br = mr; //getBonyRobot();
		if (br == null) {
			getLogger().warn("connection aborting due to missing ModelRobot, for char: {}", charID);
			return;
		}
		final HumanoidFigure hf = getHumanoidFigure(charID);
		getLogger().info("HumanoidFigure={}", hf);
		if (hf != null) {
			br.registerMoveListener(new ModelRobot.MoveListener() {
				@Override
				public void notifyBonyRobotMoved(ModelRobot br) {
					// Here the concept is to re-lookup the humanoid
					HumanoidFigure hf = getHumanoidFigure(charID);
					if (hf != null) {
						// This print() will be too copious, and will need to be disabled after bone-bugs are fixed.
						getLogger().trace("Calling propagateState for charID={}, robotID={}, hfig={}", charID, br.getRobotId(), hf);

						ModelToFigureStateMappingFuncs.propagateState(br, hf);
					} else {
						// TODO:  Again, too copious, this will semi-cripple the runtime on repeated execs
						getLogger().warn("Cannot propagate: HumanoidFigure is null for charID={}, robotID={}", charID, br.getRobotId());
					}
				}
			});
		} else {
			
		}
	}
	*/
}
