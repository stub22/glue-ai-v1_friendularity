/**
 * Copyright 2013 Hanson Robokind, LLC
 */
package org.friendularity.bundle.macro.common;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.appdapter.core.matdat.RepoSpec;
import org.appdapter.core.matdat.URLRepoSpec;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.ResourceFileCategory;
import org.cogchar.platform.util.ClassLoaderUtils;
import org.osgi.framework.BundleContext;

//import com.hrkind.content.preview.PreviewContentBundleActivator;

import ext.osgi.common.MacroBundleActivatorBase;

// normally we only use explicit exports but this gives us the ability to work with classes moved from cogchar to appdapter when smoketesting vs release

/**
 * @author Stu B. <www.texpedient.com>
 */
public class CommonMediator extends PumaContextMediator {

	// Override base class methods to customize the way that PUMA boots + runs, and
	// to receive notifications of progress during the boot / re-boot process.

	/// Format = goog:/Document/Namespace/Directory
	static final String TEST_REPO_SHEET_LOCATION = "goog:/0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc/9/8";

	static final String BOOT_CONFIG_PROP_KEY = "puma.boot.config.local";

	static public boolean myFlag_FoundBootConfigProp = false;
	static public String myRepoSheetLocalPath = TEST_REPO_SHEET_LOCATION;

	static RepoSpec myRepoSpec = null;

	@Override public RepoSpec getMainConfigRepoSpec() {
		synchronized (this) {
			if (myRepoSpec == null) {
				myRepoSpec = makeMainConfigRepoSpec();
			}
			return myRepoSpec;
		}
	}

	private RepoSpec makeMainConfigRepoSpec() {
		List<ClassLoader> fileResModelCLs = getFileResClassLoaders();
		String localBootConfigPath = myRepoSheetLocalPath;
		if (localBootConfigPath != null) {
			myFlag_FoundBootConfigProp = true;
			myRepoSheetLocalPath = localBootConfigPath;
		}
		RepoSpec rs = new URLRepoSpec(localBootConfigPath, fileResModelCLs);
		getLogger().info(this.getClass().getSimpleName() + " made RepoSpec: {} ", rs);
		return rs;
	}

	public static boolean HEADLESS = false;

	public static PumaAppContext myDemoPACtx;

	/**
	 * Boot callback sequence #1. The mediator should now do any special init that it wants to, but without assuming GUI exists. This stage includes getting the FIRST whack (both read and write) at the configuration services. This should happen before the mediator is required to answer the getFlags and getMainConfigIdent, getModeIdent kinds of questions. However, the default mediator impl does nothing. If you want the whole application to fail and shut down, you can signal that by allowing an exception to escape.
	 * 
	 * @param ctx
	 * @throws Throwable
	 */
	public void notifyContextBuilt(PumaAppContext ctx) throws Throwable {
		myDemoPACtx = ctx;

	}

	/**
	 * Boot callback sequence #2 - optional. Could apply to any GUI concepts, but currently applies only to our VWorld OpenGL panel. This callback is currently *not* invoked if there is no VWorld startup (as controlled by VWorld flag below). Called after panels constructed but before startOpenGLCanvas. Allows GUI to intervene and realize the panels as needed. This step is normally used when getFlagAllowJFrames returns false, e.g. when we are running under NB platform and we want to manage windows manually).
	 * 
	 * This callback comes after notifyContextBuilt, only if panels are actually constructed, as noted above.
	 * 
	 * If you want the whole application to fail and shut down, you can signal that by allowing an exception to escape.
	 * 
	 * @param ctx
	 * @throws Throwable
	 */
	public void notifyPanelsConstructed(PumaAppContext ctx) throws Throwable {
		myDemoPACtx = ctx;
		/*logInfo("******************* Registering HRK Preview resource bundle with default AssetContext");
		AssetContext defAssetCtx = RenderRegistryFuncs.findOrMakeAssetContext(null, null, PreviewContentBundleActivator.class);
		JmonkeyAssetLocation jmal = new JmonkeyAssetLocation(PreviewContentBundleActivator.class);
		org.appdapter.core.boot.ClassLoaderUtils.registerClassLoader(myContext, PreviewContentBundleActivator.class.getClassLoader(), "com.hrkind.content");
		org.appdapter.core.boot.ClassLoaderUtils.registerClassLoader(myContext, PreviewContentBundleActivator.class, "com.hrkind.content");
		defAssetCtx.addAssetSource(jmal);
		logInfo("******************* Completed registration of HRK Preview resource bundle with default AssetContext");
		*/
		//PumaVirtualWorldMapper pvwm = myDemoPACtx.getPumaRegistryClient().getVWorldMapper(null);
		//	for (ClassLoader cl : getFileResClassLoaders()) {
		//pvwm.connectVisualizationResources(cl);
		//	}
	}

	/**
	 * Boot callback sequence #3. The Puma chars have now been loaded, or reloaded. If you want to fix up your chars as a group, this is a good opportunity to do so. This might be called repeatedly if user re-loads the character system repeatedly, without a full reboot (and hence without the other callbacks necessarily repeating).
	 * 
	 * @param ctx
	 * @throws Throwable
	 */
	public void notifyCharactersLoaded(PumaAppContext ctx) throws Throwable {
		myDemoPACtx = ctx;
	}

	/**
	 * Boot callback sequence #4. Last thing that happens before result processing during a boot is this callback. Allows app to make sure it gets the last word on config + state.
	 * 
	 * @param ctx
	 * @throws Throwable
	 */
	@Override public void notifyBeforeBootComplete(PumaAppContext ctx) throws Throwable {
		myDemoPACtx = ctx;
	}

	public String getOptionalFilesysRoot() {
		return (new File(".")).getAbsolutePath();
	}

	public boolean getFlagAllowJFrames() {
		return !HEADLESS;
	}

	public boolean getFlagIncludeVirtualWorld() {
		boolean virtualWorld = MacroBundleActivatorBase.macroStartupSettings.isEnabled("VirtualWorld");
		return virtualWorld;
	}

	public boolean getFlagIncludeWebServices() {
		boolean lifterLifecycle = MacroBundleActivatorBase.macroStartupSettings.isEnabled("LifterLifecycle");
		return lifterLifecycle;
	}

	public boolean getFlagIncludeCharacters() {
		return true;
	}

	protected BundleContext myContext;

	public CommonMediator(BundleContext ctx) {
		myContext = ctx;
		String localBootConfigPath = System.getProperty(BOOT_CONFIG_PROP_KEY, java.lang.System.getenv(BOOT_CONFIG_PROP_KEY));
		if (localBootConfigPath != null) {
			myFlag_FoundBootConfigProp = true;
			myRepoSheetLocalPath = localBootConfigPath;
		}
	}

	@Override public List<ClassLoader> getExtraResFileCLsForCat(ResourceFileCategory cat) {
		List<ClassLoader> extraCLs = new ArrayList<ClassLoader>();
		// MAYBE LATER extraCLs.add(com.hrkind.content.preview.PreviewContentBundleActivator.class.getClassLoader());
		// Add the local loader to pick up our test CSV files.
		extraCLs.add(this.getClass().getClassLoader());
		return extraCLs;
	}

	public List<ClassLoader> getFileResClassLoaders() {
		List<ClassLoader> fileResModelCLs;
		if (myContext == null) {
			fileResModelCLs = new ArrayList<ClassLoader>();
		} else {
			fileResModelCLs = ClassLoaderUtils.getFileResourceClassLoaders(myContext, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
		}/*
			import org.cogchar.bundle.render.resources.ResourceBundleActivator;
			import com.hrkind.content.preview.PreviewContentBundleActivator;
			fileResModelCLs.add(ResourceBundleActivator.class.getClassLoader());
			fileResModelCLs.add(PreviewContentBundleActivator.class.getClassLoader());*/
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		if (cl != null)
			fileResModelCLs.add(cl);
		HashSet clset = new HashSet();
		clset.addAll(fileResModelCLs);
		fileResModelCLs.clear();
		fileResModelCLs.addAll(clset);

		return fileResModelCLs;
	}

}
