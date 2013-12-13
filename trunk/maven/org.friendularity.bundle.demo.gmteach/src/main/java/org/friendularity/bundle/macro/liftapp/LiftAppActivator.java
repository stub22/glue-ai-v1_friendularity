package org.friendularity.bundle.macro.liftapp;

import org.cogchar.app.puma.web.LifterLifecycle;
import org.cogchar.bundle.app.puma.GruesomeTAProcessingFuncs;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.joswrap.RepoUpdateCallbackAdapter;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;

import com.hp.hpl.jena.rdf.model.ModelChangedListener;

public class LiftAppActivator extends CommonActivator {

	private static ModelChangedListener theExperimentalModelListener;

	private void setupRepoUpdateCallback() {
		RepoUpdateCallbackAdapter.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
			public void repoUpdateCompleted() {
				getLogger().info("c.h.b.oglweb.R50 activator got SPARQL-UPDATE callback");
				GruesomeTAProcessingFuncs.processPendingThingActions();
			}
		});
	}

	/**
	 * Copied from Activator for Friendularity.Liftoff.
	 *
	 * @param m_context
	 */
	private void initLifterWebappLifecycle(BundleContext context0) {
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
		OSGiComponent lifterComp = new OSGiComponent(m_context, lifecycle);
		lifterComp.start();
	}

	public void registerServices(BundleContext context0) {
		addMacroService("LifterLifecycle", new Runnable() {
			@Override public void run() {
				initLifterWebappLifecycle(m_context);
			}
		});

		addMacroService("LifterPuma", new Runnable() {
			public void run() {
				configPuma();
				bootPuma();
				addMacroService("checkAnimFiles", new Runnable() {
					@Override public void run() {
						PumaAppUtils.checkAnimationFiles(null);
					}
				});
			}
		});

		addMacroService("setupRepoUpdateCallback", new Runnable() {

			@Override public void run() {
				setupRepoUpdateCallback();
			}
		});
	}

	protected CommonMediator getMediator() {
		return new CommonMediator(m_context) {
			@Override public boolean getFlagIncludeWebServices() {
				return true;
			}
		};
	}
}
