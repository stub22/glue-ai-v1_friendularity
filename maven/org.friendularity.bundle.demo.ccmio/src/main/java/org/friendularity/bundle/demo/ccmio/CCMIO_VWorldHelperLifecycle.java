package org.friendularity.bundle.demo.ccmio;

/**
 *
 * @author
 */
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import org.cogchar.api.humanoid.FigureConfig;

import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.app.puma.registry.PumaRegistryClient;
import org.cogchar.app.puma.event.CommandEvent;
import org.cogchar.app.puma.event.Updater;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaContextCommandBox;
import org.cogchar.bundle.app.vworld.central.VWorldMapperLifecycle;
import org.cogchar.bundle.app.vworld.central.VWorldRegistry;
import org.cogchar.bundle.app.vworld.startup.PumaVirtualWorldMapper;
import org.jflux.api.registry.basic.BasicDescriptor;
import org.jflux.api.service.ServiceManager;
import org.jflux.api.service.binding.ServiceBinding;
import org.jflux.impl.registry.OSGiRegistry;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;

public class CCMIO_VWorldHelperLifecycle extends BasicDebugger implements ServiceLifecycle<CCMIO_VWorldHelper> {

    private final static String DEPKEY_VWorldReg		= "vworld-registry";
    
    private final static String[] theClassNameArray = {
        CCMIO_VWorldHelper.class.getName()
    };
    
    private final static ServiceDependency[] theDependencyArray = {
		makeUnaryStaticServiceDep(DEPKEY_VWorldReg, VWorldRegistry.class)
    };
	
	private static ServiceDependency makeUnaryStaticServiceDep(String regKey, Class depClazz) { 
		return new ServiceDependency(regKey, depClazz.getName(),  ServiceDependency.Cardinality.MANDATORY_UNARY, 
				ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP);
	}

	// Here is the callback that launches our CCMIO VWorld  "Helper" demonstration object, AFTER the
	// VWorldRegistry is loaded.
    @Override public CCMIO_VWorldHelper createService(Map<String, Object> dependencyMap) {
		getLogger().info("Creating CCMIO_VWorldHelper service");
		CCMIO_VWorldHelper helper = new CCMIO_VWorldHelper();
        VWorldRegistry vwReg = (VWorldRegistry) dependencyMap.get(DEPKEY_VWorldReg);
		PumaVirtualWorldMapper pvwm = vwReg.getVW();
		// Tell the helper about the VWorld.
		helper.setVWorldMapper(pvwm, null);
		// Do some loosely defined VWorld visualization setup.
		helper.doWermStuff();
		Bundle b = FrameworkUtil.getBundle(CCMIO_VWorldHelper.class);
        if (b != null) {
			BundleContext bundleCtx = b.getBundleContext();
			// 
			helper.finishDemoSetup(bundleCtx);
        }
		return helper;
    }

    @Override  public CCMIO_VWorldHelper handleDependencyChange(CCMIO_VWorldHelper client, String changeType, String dependencyName,
            Object dependency, Map<String, Object> availableDependencies) {
        return null;
    }

    @Override public String[] getServiceClassNames() {
        return theClassNameArray;
    }

    @Override public void disposeService(CCMIO_VWorldHelper t, Map<String, Object> map) {
        t = null;
    }

    @Override public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }
	
	public static void startHelperLifecycle(BundleContext context) {
        Map<String, ServiceBinding> bindings = new HashMap<String, ServiceBinding>();
        CCMIO_VWorldHelperLifecycle helperLifecycle = new CCMIO_VWorldHelperLifecycle();

        getBindings(bindings, helperLifecycle);

        OSGiRegistry registry = new OSGiRegistry(context);

        ServiceManager serviceManager =
                new ServiceManager(
                helperLifecycle,
                bindings,
                Collections.EMPTY_MAP,
                null);
        serviceManager.start(registry);
    }
	private static Map getBindings(Map<String, ServiceBinding> bindings, ServiceLifecycle l) {	
	 
		BasicDescriptor vwRegDesc =  new BasicDescriptor(VWorldRegistry.class.getName(), null);

		ServiceBinding vwRegBinding = new ServiceBinding((ServiceDependency) l.getDependencySpecs().get(0),
                vwRegDesc, ServiceBinding.BindingStrategy.LAZY);

        bindings.put(DEPKEY_VWorldReg, vwRegBinding);

        return bindings;
	}

}
