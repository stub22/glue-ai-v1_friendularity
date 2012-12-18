package org.friendularity.bundle.demo.ccrk;

import org.appdapter.osgi.core.BundleActivatorBase;

import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.osgi.framework.BundleContext;

import org.cogchar.blob.emit.RepoSpec;
import org.cogchar.blob.emit.OnlineSheetRepoSpec;

/**
 * This class is a bundle activator demonstrating how to start the Cogchar PUMA system, in an
 * OSGi environment.  We call such a bundle a "top" bundle, which is essentially a launchable
 * application.  This bundle can be launched using a maven command line like:
 * 
 * mvn -Prun-on-felix antrun:run
 * 
 * ...which is equivalent to a project->run command in Netbeans or Eclipse.
 * 
 * When the bundle is loaded (in activator "start"), it activates Log4J logging
 * using forceLog4jConfig(), which assumes a "log4j.properties" file-resource is
 * available on the classpath.
 *
 * In principle, this bundle should start any desired subset of Cogchar functionality, for *any* environment, including
 *		<ul><li>Cogchar behavior
 *		</li><li>Robokind animation and speech
 *		</li><li>Optional Cogchar OpenGL rendering</li></ul>

*  The actual subset is determined through the intervation of a customizable PumaContextMediator object.

* L1) This bundle does not try to start a lifter webapp, which is currently initialized
 * orthogonally to the PUMA system.  See the o.f.b.demo.liftoff project.
 * <p>
 * Other immediate problems (Updated 2012-10-15):
 *		
 * P2) To exit, a user must kill the process (e.g. with netbeans "stop", or using ps/kill/TaskManager). 
 * We do not have clean shutdown triggers in place.  We need to call stop on bundle 0, when:
 *<br/>		a) A user X-closes our "main" simulator window.
 *<br/>		b) User chooses quit command from a console GUI menu or a web GUI
 * </p>
 * @author Stu B. <www.texpedient.com>
 */
public class CCRK_DemoActivator extends BundleActivatorBase {

	@Override public void start(final BundleContext context) throws Exception {
		// Will look for log4j.properties at root of this bundle.
		// Any top-level OSGi app that wants to enable Log4J (and thereby make Jena happy, while
		// retaining the power to configure Jena's logging level) should have the dependencies
		// in our pom.xml, and call this once at startup.
		forceLog4jConfig();
		// Print some howdys
		super.start(context);
		// Register our default mediator
		DemoMediator mediator = new DemoMediator();
		PumaGlobalPrebootInjector injector = PumaGlobalPrebootInjector.getTheInjector();
		// False => Do not overwrite, so any other customer mediator will get preference.
		// Our DemoMediator coded below is only used as a backup/default.
		injector.setMediator(mediator, false);
		// Schedule our callback to the handle method below.
		scheduleFrameworkStartEventHandler(context);

	}
	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		getLogger().info("Calling startPumaDemo()");
		startPumaDemo(bundleCtx);
		
		// Here we *could start some extra app-specific (e.g. Cogbot binding) goodies, and tell them to attach to 
		// PUMA  behavior system.  However, the Cogchar config system is intended to be sufficiently general to
		// handle most initialization cases without help from bundle activators.		
	}
    @Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
    }

	private void startPumaDemo(BundleContext bundleCtx) {
		PumaBooter pumaBooter = new PumaBooter();
		PumaContextMediator mediator = PumaGlobalPrebootInjector.getTheInjector().getMediator();
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(bundleCtx, mediator);
		getLogger().info("Got PUMA BootResult: " + bootResult);
	}
	static class DemoMediator extends PumaContextMediator {
		// Override base class methods to customize the way that PUMA boots + runs, and
		// to receive notifications of progress during the boot / re-boot process.
		String TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc";
		int  DFLT_NAMESPACE_SHEET_NUM = 9;
		int   DFLT_DIRECTORY_SHEET_NUM = 8;
		
		@Override public RepoSpec getMainConfigRepoSpec() {
			java.util.List<ClassLoader> fileResModelCLs = new java.util.ArrayList<ClassLoader>();
			return new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, DFLT_DIRECTORY_SHEET_NUM,
							fileResModelCLs);
		}
	}	

}