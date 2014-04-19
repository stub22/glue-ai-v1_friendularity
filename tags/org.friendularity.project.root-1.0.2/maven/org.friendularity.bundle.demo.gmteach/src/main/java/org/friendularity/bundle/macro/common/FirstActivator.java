package org.friendularity.bundle.macro.common;

import org.osgi.framework.BundleContext;

import ext.osgi.common.ExtBundleActivatorBase;

public class FirstActivator extends ExtBundleActivatorBase {

	public FirstActivator() {
		MACRO_LAUNCHER = true;
	}

	@Override public void start(BundleContext bundleCtx) throws Exception {
		CommonActivator.MACRO_LAUNCHER = true;
		super.start(bundleCtx);
	}
}
