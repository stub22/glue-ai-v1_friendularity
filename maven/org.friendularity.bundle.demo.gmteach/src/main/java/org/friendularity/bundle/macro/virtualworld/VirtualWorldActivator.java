/**
 * Copyright 2012 Hanson Robokind, LLC
 */

package org.friendularity.bundle.macro.virtualworld;

import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.osgi.framework.BundleContext;
import org.robokind.api.motion.Robot;

/**
 * Starting code copied from Friendularity's CCRK_DemoActivator
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class VirtualWorldActivator extends CommonActivator {

	@Override
	public void registerServices(BundleContext context0) {
		addMacroService("VirtualWorld", new Runnable() {
			@Override
			public void run() {
				startPumaDemo(m_context);
			}
		});
	}

	@Override
	protected void launchApplication(BundleContext bundleCtx) throws Exception {
		startPumaDemo(bundleCtx);
	}

	public void startPumaDemo(BundleContext bundleCtx) {
		configPuma();
		bootPuma();
		Robot.Id optRobotID_elseAllRobots = null;
		PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(bundleCtx,
				optRobotID_elseAllRobots);
		addMacroService("checkAnimFiles", new Runnable() {
			@Override
			public void run() {
				PumaAppUtils.checkAnimationFiles(System.err);
			}
		});
	}

	protected CommonMediator getMediator() {
		return new VirtualWorldOnlyDemoMediator(m_context);
	}

	static public class VirtualWorldOnlyDemoMediator extends CommonMediator {

		public VirtualWorldOnlyDemoMediator(BundleContext ctx) {
			super(ctx);
		}

		@Override
		public boolean getFlagIncludeVirtualWorld() {
			return true;
		}
	}

}
