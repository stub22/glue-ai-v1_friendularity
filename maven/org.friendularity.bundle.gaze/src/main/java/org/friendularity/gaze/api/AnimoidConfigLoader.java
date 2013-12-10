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

package org.friendularity.gaze.api;


import org.cogchar.api.animoid.config.bonus.AnimationBlendConfig;
import org.cogchar.sight.api.facerec.FreckleMatchConfig;
import org.cogchar.sight.api.facerec.FaceNoticeConfig;
import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;
import java.net.URL;
import org.apache.log4j.BasicConfigurator;



import org.friendularity.gaze.api.GazeJoint;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.friendularity.gaze.api.GlanceStrategy;
import org.friendularity.gaze.api.StereoGazeConfig;
import org.cogchar.sight.api.core.SightPort;
import org.cogchar.api.animoid.protocol.Robot;
import org.cogchar.api.animoid.world.WorldJoint;
import org.dom4j.io.SAXReader;
import org.cogchar.impl.perform.DummyTextChan;
import org.friendularity.gaze.util.GazeStrategyCue;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class AnimoidConfigLoader {
	// public static String testFilename = 	"animoid\\animoid_bina.xml";
	
	public static XStream buildDom4jXStreamForRead() {
		Dom4JDriver dom4jDriver = new Dom4JDriver();
		XStream xstream = new XStream(dom4jDriver);
		initConfigXStream(xstream);
		return xstream;
	}
	public static void initConfigXStream(XStream xstream) {
		xstream.alias("GazeJoint", GazeJoint.class);
		xstream.alias("GazeStrategy", GazeStrategyCue.class);
		xstream.alias("GazeJointStrategy", GazeJointStrategy.class);
		xstream.alias("AnimoidConfig", AnimoidConfig.class);
		xstream.alias("GlanceStrategy", GlanceStrategy.class);
		xstream.alias("FaceNoticeConfig", FaceNoticeConfig.class);
		xstream.alias("StereoGazeConfig", StereoGazeConfig.class);
		xstream.alias("FreckleMatchConfig", FreckleMatchConfig.class);
		xstream.alias("AnimationBlendConfig", AnimationBlendConfig.class);

		
		xstream.addImplicitCollection(AnimoidConfig.class, "myGazeStrategies", GazeStrategyCue.class);
		xstream.addImplicitCollection(AnimoidConfig.class, "myGazeJoints", GazeJoint.class);
		xstream.addImplicitCollection(GazeStrategyCue.class, "myJointLinks", GazeJointStrategy.class);
		xstream.useAttributeFor(GazeJoint.class, "positiveDirection");
		xstream.useAttributeFor(WorldJoint.class, "logicalJointID");
		xstream.useAttributeFor(WorldJoint.class, "rangeOfMotionDegrees");
		xstream.useAttributeFor(GazeStrategyCue.class, "name");
		xstream.useAttributeFor(GazeStrategyCue.class, "motionStyle");
		xstream.useAttributeFor(GazeJointStrategy.class, "logicalJointID");
		xstream.useAttributeFor(SightPort.class, "widthPixels");
		xstream.useAttributeFor(SightPort.class, "widthDegrees");
		xstream.useAttributeFor(SightPort.class, "heightPixels");
		xstream.useAttributeFor(SightPort.class, "heightDegrees");
		xstream.useAttributeFor(SightPort.class, "azSkewDegrees");
		xstream.useAttributeFor(SightPort.class, "elSkewDegrees");
			
		xstream.aliasField("GlanceStrategy", GazeStrategyCue.class, "glanceStrategy");
		xstream.aliasField("ViewPort", AnimoidConfig.class, "myViewPort");
		xstream.aliasField("FaceNoticeConfig", AnimoidConfig.class, "myFaceNoticeConfig");		
		xstream.aliasField("StereoGazeConfig", AnimoidConfig.class, "myStereoGazeConfig");
		xstream.aliasField("FreckleMatchConfig", AnimoidConfig.class, "myFreckleMatchConfig");
		xstream.aliasField("AnimationBlendConfig", AnimoidConfig.class, "myAnimationBlendConfig");
		/* Switched to elements for these to facilitate commenting in the XML
		xstream.useAttributeFor(FaceNoticeConfig.class, "initialStrength");
		...
		*/
	}
	public static AnimoidConfig loadAnimoidConfig(URL configFileURL, Robot mainRobot,
				Integer msecPerFrame, Double frameSmoothingFactor) throws Throwable {

		XStream xstream = buildDom4jXStreamForRead();
		// FileReader fread = new FileReader(configFileURL);
	
		// This URL based arg requires XStream 1.4 - we could open the stream manuallly if we want to fall
		// back to 1.3. (e.g. to use servicemix OSGi bundle for XStream).
		AnimoidConfig animoidConfig = (AnimoidConfig) xstream.fromXML(configFileURL); //fread);
		if (mainRobot != null) {
			animoidConfig.completeInit(mainRobot, msecPerFrame, frameSmoothingFactor);
		}
		return animoidConfig;
	}
	
	public static void main(String[] args) {
		try { 
			String testPath = "/org/cogchar/test/animoid/animoid_zeno_robokind.xml";
			URL testURL = AnimoidConfigLoader.class.getResource(testPath);
			System.out.println("resolved URL: " + testURL);
			
        SAXReader reader = new SAXReader();
			// Document document = reader.read(testURL);
			// System.out.println("Read doc: " + document.asXML());
			// Dom4JReader d4jr = new Dom4JReader(document, new XmlFriendlyReplacer());
			
			AnimoidConfig animoidConfig = loadAnimoidConfig(testURL, null, 100, 1.3);
			
			System.out.println("Loaded animoidConfig: " + animoidConfig);
		} catch (Throwable t) {
			System.err.println("Caught: " + t);
			t.printStackTrace();
		}
	}
}
