/*
 * Copyright 2014 the Friendularity Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.friendularity.bundle.macro.tools;

import org.jflux.impl.services.rk.lifecycle.ManagedService;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponentFactory;
import org.mechio.api.common.osgi.lifecycle.ConfiguredServiceLifecycle;
import org.mechio.api.common.osgi.lifecycle.ConfiguredServiceParams;
import org.mechio.api.common.services.Constants;
import org.mechio.api.motion.Robot;
import org.mechio.api.motion.lifecycle.RobotJointGroupLifecycle;
import org.mechio.api.motion.servos.ServoRobot;
import org.mechio.api.motion.servos.config.ServoRobotConfig;
import org.mechio.api.motion.servos.utils.ServoRobotLifecycle;
import org.mechio.api.motion.utils.RobotUtils;
import org.mechio.extern.utils.apache_commons_configuration.ConfigUtils;
import org.mechio.impl.motion.config.RobotConfigXMLReader;
import org.mechio.impl.motion.jointgroup.RobotJointGroupConfigXMLReader;
import org.mechio.impl.motion.lifecycle.RemoteRobotHostServiceGroup;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import java.io.File;
import java.util.Properties;

/**
 * @author Matthew Stevenson <www.friendularity.org>
 */
public class RobotStart {
	private static final org.slf4j.Logger theLogger = org.slf4j.LoggerFactory.getLogger(RobotStart.class);

	public static void startRobot(
			BundleContext context, Robot.Id robotId,
			String robotXMLConfigPath, String jointGroupXMLConfigPath,
			String connectionConfigId, long blenderInterval) {
		if (context == null) {
			theLogger.warn(
					"Unable to load Robot.  Could not find BundleContext.");
			return;
		}
		launchRobot(context, robotId, robotXMLConfigPath);
		RobotUtils.startDefaultBlender(
				context, robotId, blenderInterval);
		loadJointGroup(context, robotId, jointGroupXMLConfigPath);
		new RemoteRobotHostServiceGroup(
				context, robotId, "host", "client",
				connectionConfigId, null).start();
	}

	protected static ServiceRegistration loadJointGroup(
			BundleContext context, Robot.Id robotId, String configPath) {
		File file = ConfigUtils.getFileSystemAdapter().openFile(configPath);
		if (file == null) {
			return null;
		}
		String paramId = "robot/" + robotId + "/jointgroup/config/param/xml";
		launchJointGroupConfig(context, file, paramId);
		RobotJointGroupLifecycle<File> lifecycle =
				new RobotJointGroupLifecycle<File>(robotId, File.class,
						paramId, RobotJointGroupConfigXMLReader.VERSION);
		OSGiComponent jointGroupComp = new OSGiComponent(context, lifecycle);
		jointGroupComp.start();
		return null;
	}

	private static OSGiComponent launchJointGroupConfig(
			BundleContext context, File file, String paramId) {
		Properties props = new Properties();
		props.put(Constants.CONFIG_PARAM_ID, paramId);
		props.put(Constants.CONFIG_FORMAT_VERSION,
				RobotJointGroupConfigXMLReader.VERSION.toString());
		ServiceLifecycleProvider lifecycle =
				new SimpleLifecycle(file, File.class, props);
		OSGiComponent paramComp = new OSGiComponent(context, lifecycle);
		paramComp.start();
		return paramComp;
	}

	private static ManagedService launchRobot(
			BundleContext context, Robot.Id robotId, String path) {
		File file = ConfigUtils.getFileSystemAdapter().openFile(path);
		if (file == null) {
			return null;
		}
		String paramId = "robot/" + robotId + "/config/param/xml";
		launchRobotConfig(context, file, paramId);
		return loadRobotService(context, robotId, paramId);
	}

	protected static ManagedService loadRobotService(
			BundleContext context, Robot.Id robotId, String paramId) {
		ConfiguredServiceParams<Robot, ServoRobotConfig, File> params =
				new ConfiguredServiceParams(
						Robot.class,
						ServoRobotConfig.class,
						File.class,
						null, null, paramId,
						ServoRobot.VERSION,
						RobotConfigXMLReader.VERSION);
		ConfiguredServiceLifecycle lifecycle =
				new ServoRobotLifecycle(
						params, new OSGiComponentFactory(context));

		Properties props = new Properties();
		props.put(Robot.PROP_ID, robotId.getRobtIdString());
		OSGiComponent robotComponent =
				new OSGiComponent(context, lifecycle, props);
		robotComponent.start();
		return robotComponent;
	}

	private static OSGiComponent launchRobotConfig(
			BundleContext context, File file, String paramId) {
		Properties props = new Properties();
		props.put(Constants.CONFIG_PARAM_ID, paramId);
		props.put(Constants.CONFIG_FORMAT_VERSION,
				RobotConfigXMLReader.VERSION.toString());
		ServiceLifecycleProvider lifecycle =
				new SimpleLifecycle(file, File.class, props);
		OSGiComponent paramComp = new OSGiComponent(context, lifecycle);
		paramComp.start();
		return paramComp;
	}
}
