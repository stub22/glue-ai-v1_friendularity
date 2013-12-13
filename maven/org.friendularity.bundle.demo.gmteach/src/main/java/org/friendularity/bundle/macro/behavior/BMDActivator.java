package org.friendularity.bundle.macro.behavior;

import java.util.logging.Level;

import javax.swing.JLabel;
import javax.swing.UIManager;

import org.appdapter.core.matdat.EnhancedRepoClient;
import org.appdapter.core.matdat.RepoSpec;
import org.appdapter.core.store.Repo;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.impl.scene.read.BehavMasterConfigTest;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.jflux.api.core.config.Configuration;
import org.jflux.api.core.config.DefaultConfiguration;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;
import org.rwshop.swing.common.config.ConfigFrame;
import org.rwshop.swing.common.config.ConfigListFrame;
import org.rwshop.swing.common.config.ConfigListPanel;
import org.rwshop.swing.common.config.ConfigurationTracker;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

// import org.cogchar.outer.behav.demo.RepoConnector;

public class BMDActivator extends CommonActivator {

	@Override public void registerServices(BundleContext context0) {

		//************************************************************************************************
		// With this call enabled, we get a Permgen during Oglweb.r25 init on JDK6-Win7x64 with -XX:MaxPermSize=512M.
		// With it disabled, we are hunky-dory?
		addMacroService("launchBehaviorLifecycles", new Runnable() {
			@Override public void run() {
				launchBehaviorLifecycles(m_context);
			}
		});

		//************************************************************************************************

		addMacroService("ServicePanel", new Runnable() {
			@Override public void run() {
				startServicePanel(m_context);
			}
		});

		addMacroPreService("useBMDRepos", new Runnable() {
			@Override public void run() {
				installSharedRepo(m_context);
			}
		});

		addMacroService("launchTriggerPanel", new Runnable() {
			@Override public void run() {
				startTriggerPanelSeparately(m_context);
			}
		});

		addMacroService("ConfigFrame", new Runnable() {
			@Override public void run() {
				ConfigFrame sf = new ConfigFrame();
				ConfigurationTracker ct = new ConfigurationTracker(m_context);
				ConfigListFrame cltf = new ConfigListFrame();
				ConfigListPanel list = cltf.getList();

				ct.setList(list);
				list.setConfigFrame(sf);

				sf.setVisible(true);
				ct.start();
				cltf.setVisible(true);
			}
		});

		addMacroService("addConfigDemo", new Runnable() {
			@Override public void run() {

				addConfigs(m_context);
			}
		});

		// perhaps use a system property instead
		// setLookAndFeel();

	}

	private void addConfigs(BundleContext context) {

		DefaultConfiguration<String> inConfig = new DefaultConfiguration<String>();
		inConfig.addProperty(Integer.class, "intProp", 4);
		inConfig.addProperty(Double.class, "dblProp", 4.0);
		inConfig.addProperty(Byte.class, "byteProp", (byte) 4);
		inConfig.addProperty(String.class, "strProp2", "four");
		inConfig.addProperty(JLabel.class, "otherProp", new JLabel("four"));
		inConfig.addProperty(DefaultConfiguration.class, "confProp", inConfig);

		DefaultConfiguration<String> config = new DefaultConfiguration<String>();
		config.addProperty(Integer.class, "intProp", 5);
		config.addProperty(Double.class, "dblProp", 5.0);
		config.addProperty(Byte.class, "byteProp", (byte) 5);
		config.addProperty(String.class, "strProp", "five");
		config.addProperty(DefaultConfiguration.class, "confProp", inConfig);
		config.addProperty(JLabel.class, "otherProp", new JLabel("five"));

		context.registerService(Configuration.class.getName(), config, null);
		context.registerService(Configuration.class.getName(), inConfig, null);
	}

	private void launchBehaviorLifecycles(BundleContext context) {

		final ServiceLifecycleProvider lifecycle;

		if (!HEADLESS) {
			lifecycle = new BehaviorMasterLifecycle(context);
			/// BehaviorMasterLifecycle code does this on it's own
			macroStartupSettings.putSetting("launchTriggerPanel", false);
		} else {
			lifecycle = new HeadlessBehaviorLifecycle(context);
		}

		new OSGiComponent(context, lifecycle).start();
	}

	private void installSharedRepo(BundleContext context) {

		BehaviorMasterConfig n = new BehaviorMasterConfig();
		boolean useURLOrFile = false;

		String envVar = macroStartupSettings.getSetting(REPO_URL_VAR);
		if (envVar != null) {
			envVar = envVar.trim();
			if (!envVar.isEmpty()) {
				n.loadDataFromConfig();
				useURLOrFile = true;
				n.setWorkbookPath(envVar);
			}
		}

		RepoSpec repoSpec;

		if (useURLOrFile) {
			// Setup to collect an OfflineSheet repo specified by POM configuration
			//  FYI: Offline = local file or URI
			repoSpec = n.makeBMC_OfflineXlsSheetRepoSpec(context);
		} else {
			// Setup to connect to a GoogSheet repo specified by POM configuration
			repoSpec = n.makeBMC_OnlineSheetRepoSpec(context);
		}

		Repo.WithDirectory bmcMemoryRepoHandle = repoSpec.makeRepo();
		EnhancedRepoClient enhancedRepoSpec = new EnhancedRepoClient(repoSpec, bmcMemoryRepoHandle, (String) BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR(),
				BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN());

		context.registerService(RepoClient.class.getName(), enhancedRepoSpec, null);
	}

	private void setLookAndFeel() {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			@Override public void run() {
				try {
					UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
				} catch (Exception ex) {
				}
			}
		});
	}

	// made public for outside control
	public void startServicePanel(final BundleContext context) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override public void run() {
				if (HEADLESS)
					return;
				ServicesFrame.create(context);
			}
		});
	}

	// made public for outside control
	public void startTriggerPanelSeparately(final BundleContext context) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override public void run() {
				if (HEADLESS)
					return;
				TriggerFrame tf = new TriggerFrame();
				String adminPanelId = "adminID";
				String behaviorPanelId = "behaviorID";
				tf.init(context, behaviorPanelId, adminPanelId);
				tf.setVisible(true);
			}
		});
	}

	public void initLogging() {
		// Expects log4j.properties in the root of this bundle.
		// In Netbeans, look under "Other Sources"/"<default package>"
		// In the filesystem, look under src/main/resources
		forceLog4jConfig();

		java.util.logging.Logger.getLogger(org.jflux.impl.services.rk.osgi.lifecycle.ServiceDependenciesTracker.class.getName()).setLevel(Level.WARNING);
	}

}
