/**
 * Copyright 2013 Hanson Robokind, LLC
 */
package org.friendularity.bundle.macro.common;

import java.util.ArrayList;
import java.util.List;

import org.appdapter.core.matdat.RepoSpec;
import org.appdapter.core.matdat.URLRepoSpec;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.ResourceFileCategory;
import org.cogchar.platform.util.ClassLoaderUtils;
import org.osgi.framework.BundleContext;

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

	@Override
	public RepoSpec getMainConfigRepoSpec() {
		List<ClassLoader> fileResModelCLs = getFileResClassLoaders();
		String localBootConfigPath = System.getProperty(BOOT_CONFIG_PROP_KEY,
				java.lang.System.getenv(BOOT_CONFIG_PROP_KEY));
		if (localBootConfigPath != null) {
			myFlag_FoundBootConfigProp = true;
			myRepoSheetLocalPath = localBootConfigPath;
		}
		RepoSpec rs = new URLRepoSpec(localBootConfigPath, fileResModelCLs);
		getLogger().info(
				this.getClass().getSimpleName() + " made RepoSpec: {} ", rs);
		return rs;
	}

	public static boolean HEADLESS = false;

	public PumaAppContext myDemoPACtx;

	@Override
	public void notifyBeforeBootComplete(PumaAppContext ctx) throws Throwable {
		myDemoPACtx = ctx;
	}

	public boolean getFlagAllowJFrames() {
		return !HEADLESS;
	}

	public boolean getFlagIncludeVirtualWorld() {
		return false;
	}

	public boolean getFlagIncludeWebServices() {
		return false;
	}

	public boolean getFlagIncludeCharacters() {
		return true;
	}

	protected BundleContext myContext;

	public CommonMediator(BundleContext ctx) {
		myContext = ctx;
		String localBootConfigPath = System.getProperty(BOOT_CONFIG_PROP_KEY,
				java.lang.System.getenv(BOOT_CONFIG_PROP_KEY));
		if (localBootConfigPath != null) {
			myFlag_FoundBootConfigProp = true;
			myRepoSheetLocalPath = localBootConfigPath;
		}
	}

	@Override
	public List<ClassLoader> getExtraResFileCLsForCat(ResourceFileCategory cat) {
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
			fileResModelCLs = ClassLoaderUtils.getFileResourceClassLoaders(
					myContext, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
		}
		return fileResModelCLs;
	}

}
