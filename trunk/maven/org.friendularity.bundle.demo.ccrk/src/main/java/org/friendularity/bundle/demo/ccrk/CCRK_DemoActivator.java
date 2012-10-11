package org.friendularity.bundle.demo.ccrk;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.cogchar.bundle.app.puma.PumaAppContext;

import org.cogchar.bundle.app.puma.PumaBooter;
import org.cogchar.bundle.app.puma.PumaContextMediator;
import org.osgi.framework.BundleContext;

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
 * In principle, this bundle should start *all* of Cogchar, for *any* environment, including
 *		<ul><li>Cogchar behavior
 *		</li><li>Robokind animation and speech
 *		</li><li>Optional Cogchar OpenGL rendering</li></ul>
 * 
 * wever, in practice, at this time (reviewed 2012-09-28), our limitations are:
 * 
 * L1) This bundle does not try to start a lifter webapp, which is currently initialized
 * orthogonally to the PUMA system.  See the o.f.b.demo.liftoff project.
 * <p>
 * Other immediate problems (2012-09-28):
 * P1) At present the PumaBooter is hardcoded to *always* init an OpenGL simulator window.
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
		DemoMediator mediator = new DemoMediator();
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(bundleCtx, mediator);
		getLogger().info("Got PUMA BootResult: " + bootResult);
	}
	static class DemoMediator extends PumaContextMediator {
		// These methods can be customized to change the way that PUMA boots + runs, and
		// to receive notifications of progress during the boot / re-boot process.
		
		@Override public boolean getFlagAllowJFrames() {
			return super.getFlagAllowJFrames();
		}

		@Override public boolean getFlagIncludeCharacters() {
			return super.getFlagIncludeCharacters();
		}

		@Override public boolean getFlagIncludeVirtualWorld() {
			return super.getFlagIncludeVirtualWorld();
		}

		@Override public boolean getFlagIncludeWebServices() {
			return super.getFlagIncludeWebServices();
		}

		@Override public String getOptionalFilesysRoot() {
			return super.getOptionalFilesysRoot();
		}

		@Override public String getPanelKind() {
			return super.getPanelKind();
		}

		@Override public String getSysContextRootURI() {
			return super.getSysContextRootURI();
		}

		@Override public void notifyContextBuilt(PumaAppContext ctx) throws Throwable {
			super.notifyContextBuilt(ctx);
		}

		@Override public void notifyPanelsConstructed(PumaAppContext ctx) throws Throwable {
			super.notifyPanelsConstructed(ctx);
		}
		@Override public void notifyCharactersLoaded(PumaAppContext ctx) throws Throwable {
			super.notifyCharactersLoaded(ctx);
		}
		
		@Override public void notifyBeforeBootComplete(PumaAppContext ctx) throws Throwable {
			super.notifyBeforeBootComplete(ctx);
		}
		
	}	

}
