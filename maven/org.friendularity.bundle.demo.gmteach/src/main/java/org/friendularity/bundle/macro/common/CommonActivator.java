/**
 * Copyright 2013 Hanson Robokind, LLC
 */

package org.friendularity.bundle.macro.common;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.log4j.PropertyConfigurator;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

import ext.osgi.common.MacroBundleActivatorBase;
import org.appdapter.osgi.core.BundleActivatorBase;

/*
 *
 *
 * Starting code copied from Friendularity's CCRK_DemoActivator
 *
 * @author Stu B. <www.texpedient.com>
 *  @author DMiles@logicmoo.org
 */
public abstract class CommonActivator extends BundleActivatorBase {

	public final static String ROBOT_CONNECTION_ENV_VAR_KEY = "com.hrkind.robot.connections";
	// mimic the repo semantics we've been using in PUMA Boots
	public static final String REPO_URL_VAR = "puma.boot.config.local";
	public static final String BM_TYPE_VAR = "behavior.master.type";
	public static final String BM_TYPE_HEADLESS = "headless";
	public static final String BM_TYPE_SWING = "swing";
	public static boolean HEADLESS = false;

	public CommonActivator() {
		String bmEnvVar = System.getProperty(BM_TYPE_VAR, System.getenv(BM_TYPE_VAR));
		if (bmEnvVar != null) {
			bmEnvVar = bmEnvVar.trim();
			if (BM_TYPE_HEADLESS.equals(bmEnvVar)) {
				macroStartupSettings.putSetting("launchGUI", false);
			}
		}
	}

	public void start(BundleContext bundleCtx) throws Exception {
		super.start(bundleCtx);
	}

	public static void doubug(String message) {
		//getLoggerForClass(CommonActivator.class).warn(message);
		// new Exception(message).fillInStackTrace().printStackTrace();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}


}