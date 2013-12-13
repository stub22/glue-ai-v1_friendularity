/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.tools;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.jflux.api.core.config.Configuration;
import org.jflux.api.core.config.DefaultConfiguration;
import org.jflux.impl.services.rk.lifecycle.utils.ManagedServiceFactory;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponentFactory;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.utils.RKConstants;
import org.robokind.api.motion.Robot;
import org.robokind.impl.messaging.config.RKMessagingConfigUtils;
import org.robokind.impl.speech.RemoteSpeechUtils;
import org.robokind.integration.motion_speech.VisemeMotionUtils;

/**
 *
 * @author matt
 */
public class R50ConfigUtils {
	public final static String ROBOT_ID = "robotId";
	public final static String SPEECH_SERVICE_ID = "speechServiceId";
	public final static String ROBOT_CONNECT_CONFIG_ID = "robotConnect";
	public final static String SPEECH_CONNECT_CONFIG_ID = "speechConnect";
	public final static String IP_ADDRESS = "ipAddrs";
	public final static String ROBOT_XML_CONFIG_PATH = "robotConfig";
	public final static String JOINT_GROUP_XML_CONFIG_PATH = "jointGroupConfig";
	public final static String VISEME_JSON_CONFIG_PATH = "visemeConfig";
	public final static String BLENDER_INTERVAL = "blenderInterval";

	public final static String SPEECH_DEFAULT_PREFIX = "speech";
	public final static String EXTRA_VOICES_ENV_VAR_KEY = "RKExtraVoices";
	public final static String EXTRA_VOICES_DELIM = ":";

	public static Configuration<String> getDefaultR50Config() {
		DefaultConfiguration<String> conf = new DefaultConfiguration<String>();
		conf.addProperty(Robot.Id.class, ROBOT_ID, new Robot.Id(RKConstants.PHYSICAL_R50_ID));
		conf.addProperty(String.class, SPEECH_SERVICE_ID, RKConstants.DEFAULT_SPEECH_ID);
		conf.addProperty(String.class, ROBOT_CONNECT_CONFIG_ID, "remoteRobotConnectionConfig");
		conf.addProperty(String.class, SPEECH_CONNECT_CONFIG_ID, "speechServiceConnectionConfig");
		conf.addProperty(String.class, IP_ADDRESS, "127.0.0.1");
		conf.addProperty(String.class, ROBOT_XML_CONFIG_PATH, "/home/fit/robokind/resources/robot.xml");
		conf.addProperty(String.class, JOINT_GROUP_XML_CONFIG_PATH, "/home/fit/robokind/resources/jointgroup.xml");
		conf.addProperty(String.class, VISEME_JSON_CONFIG_PATH, "/home/fit/robokind/resources/VisemeConf.json");
		conf.addProperty(Long.class, BLENDER_INTERVAL, 40L);
		return conf;
	}

	public static void startR50(BundleContext context, Configuration<String> conf) {
		ManagedServiceFactory fact = new OSGiComponentFactory(context);
		startRobot(context, fact, conf);
		startSpeech(context, fact, conf);
		startVisemes(fact, conf);
		startExtraVoices(fact, conf);
	}

	public static void startRobot(BundleContext context, ManagedServiceFactory fact, Configuration<String> conf) {
		RKMessagingConfigUtils.registerConnectionConfig(conf.getPropertyValue(String.class, ROBOT_CONNECT_CONFIG_ID), conf.getPropertyValue(String.class, IP_ADDRESS), null, fact);

		RobotStart.startRobot(context, conf.getPropertyValue(Robot.Id.class, ROBOT_ID), conf.getPropertyValue(String.class, ROBOT_XML_CONFIG_PATH),
				conf.getPropertyValue(String.class, JOINT_GROUP_XML_CONFIG_PATH), conf.getPropertyValue(String.class, ROBOT_CONNECT_CONFIG_ID), conf.getPropertyValue(Long.class, BLENDER_INTERVAL));
	}

	public static void startSpeech(BundleContext context, ManagedServiceFactory fact, Configuration<String> conf) {
		RKMessagingConfigUtils.registerConnectionConfig(conf.getPropertyValue(String.class, SPEECH_CONNECT_CONFIG_ID), conf.getPropertyValue(String.class, IP_ADDRESS), null, fact);
		RemoteSpeechUtils.connect(new OSGiComponentFactory(context), conf.getPropertyValue(String.class, SPEECH_SERVICE_ID), SPEECH_DEFAULT_PREFIX,
				conf.getPropertyValue(String.class, SPEECH_CONNECT_CONFIG_ID));
	}

	public static void startVisemes(ManagedServiceFactory fact, Configuration<String> conf) {
		VisemeMotionUtils.startVisemeFrameSourceGroup(fact, conf.getPropertyValue(Robot.Id.class, ROBOT_ID), conf.getPropertyValue(String.class, SPEECH_SERVICE_ID),
				conf.getPropertyValue(String.class, VISEME_JSON_CONFIG_PATH));
	}

	public static void startExtraVoices(ManagedServiceFactory fact, Configuration<String> conf) {
		ExtraVoiceUtils.startExtraVoices(fact, conf.getPropertyValue(Robot.Id.class, ROBOT_ID), getExtraVoices(EXTRA_VOICES_ENV_VAR_KEY, EXTRA_VOICES_DELIM),
				conf.getPropertyValue(String.class, SPEECH_CONNECT_CONFIG_ID), conf.getPropertyValue(String.class, VISEME_JSON_CONFIG_PATH));
	}

	public static List<String> getExtraVoices(String envVarKey, String delim) {
		String envVar = System.getProperty(envVarKey, java.lang.System.getenv(envVarKey));
		System.out.println("Found Extra Voices: " + envVar);
		if (envVar == null || envVar.isEmpty()) {
			return Collections.EMPTY_LIST;
		}
		String[] vals = envVar.split(delim);
		List<String> retVals = new ArrayList<String>(vals.length);
		for (String v : vals) {
			v = v.trim();
			if (v.isEmpty()) {
				continue;
			}
			retVals.add(v);
		}
		return retVals;
	}
}
