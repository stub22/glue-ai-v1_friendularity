package org.friendularity.bundle.bento;

import org.friendularity.bundle.bento.gui.BentoLauncher;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.FrameworkListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BentoBundleActivator implements BundleActivator {
	// this was a Missing File (or didn't make it onto the SVN) 
	
	Logger theLogger = LoggerFactory.getLogger(BentoBundleActivator.class);
	private BentoLauncher myBento;

	public void start(BundleContext context) throws Exception {
		myBento = new BentoLauncher(false);
		context.addFrameworkListener(new GotFrameworkStartEvent());
	}

	final class GotFrameworkStartEvent implements FrameworkListener {
		public void frameworkEvent(FrameworkEvent fe) {
			int eventType = fe.getType();
			getLogger().info("************************ Got frameworkEvent with eventType=" + eventType + ", bundle=" + fe.getBundle());
			if (eventType == FrameworkEvent.STARTED) {
				getLogger().info("********  OSGi Framework has STARTED, calling dispatchFrameworkStartedEvent()");
				dispatchFrameworkStartedEvent(fe.getBundle(), fe.getThrowable());
			}
		}
	}

	public void stop(BundleContext context) throws Exception {
		// This handler is important in the case where *some other bundle* (outside JVision) is trying
		// to stop the JVM/OSGi process.  
		getLogger().info("Activator sending requestStop() to myLauncher, in case some *other* bundle is stopping the container.");
		myBento.requestStop(Boolean.FALSE);
	}

	public Logger getLogger() {
		return theLogger;
	}

	protected void dispatchFrameworkStartedEvent(Bundle bundle, Throwable t) {
		if (bundle != this)
			return;
		getLogger().info("In OSGi framework-started callback, initialization of BentoLauncher");
		// How to get the cmd line args if we need them
		String args = getProperty(bundle, "", "bento.args", "application.args", "launcher.arguments");
		// call main
		myBento.attemptInit();
		BentoLauncher.main(args.split(" "));
		getLogger().info("BentoLauncher complete");
	}

	private String getProperty(Bundle bundle, String defult, String... tryFrom) {
		BundleContext context = bundle.getBundleContext();
		boolean blankString = false;
		for (String s : tryFrom) {

			String args = context.getProperty(s);
			if (args != null) {
				if (args.length() == 0) {
					blankString = true;
					continue;
				}
				return args;
			}
			args = System.getProperty(s, null);
			if (args != null)
				return args;
		}
		if (blankString)
			return "";
		return defult;
	}

}
