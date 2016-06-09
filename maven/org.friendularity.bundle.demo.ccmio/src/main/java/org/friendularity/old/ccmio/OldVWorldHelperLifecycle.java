package org.friendularity.old.ccmio;

/**
 *
 * @author Major Jacquote II <mjacquote@gmail.com>
 */
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.HashMap;

import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.appdapter.core.log.BasicDebugger;
import org.cogchar.bundle.app.vworld.central.VWorldRegistry;
import org.jflux.api.registry.basic.BasicDescriptor;
import org.jflux.api.service.ServiceManager;
import org.jflux.api.service.binding.ServiceBinding;
import org.jflux.impl.registry.OSGiRegistry;
import org.osgi.framework.BundleContext;

public class OldVWorldHelperLifecycle extends BasicDebugger implements ServiceLifecycle<OldVWorldHelper> {

    private final static String DEPKEY_VWorldReg		= "vworld-registry";
    
    private final static String[] theClassNameArray = {
        OldVWorldHelper.class.getName()
    };
    
    private final static ServiceDependency[] theDependencyArray = {
		makeUnaryStaticServiceDep(DEPKEY_VWorldReg, VWorldRegistry.class)
    };
	
	private static ServiceDependency makeUnaryStaticServiceDep(String regKey, Class depClazz) { 
		return new ServiceDependency(regKey, depClazz.getName(),  ServiceDependency.Cardinality.MANDATORY_UNARY, 
				ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP);
	}

	// Here is the callback that launches our CCMIO VWorld  "Helper" demonstration object,
	// AFTER the VWorldRegistry is loaded (down in the Cogchar vworld-launch layer).
    @Override public OldVWorldHelper createService(Map<String, Object> dependencyMap) {
		getLogger().info("Creating OldVWorldHelper service");
		OldVWorldHelper helper = new OldVWorldHelper();
        VWorldRegistry vwReg = (VWorldRegistry) dependencyMap.get(DEPKEY_VWorldReg);
		helper.completeSetup(vwReg);
		return helper;
    }

    @Override  public OldVWorldHelper handleDependencyChange(OldVWorldHelper client, String changeType, String dependencyName,
															 Object dependency, Map<String, Object> availableDependencies) {
        return null;
    }

    @Override public String[] getServiceClassNames() {
        return theClassNameArray;
    }

    @Override public void disposeService(OldVWorldHelper t, Map<String, Object> map) {
        t = null;
    }

    @Override public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }
	
	public static void startHelperLifecycle(BundleContext context) {
        Map<String, ServiceBinding> bindings = new HashMap<String, ServiceBinding>();
        OldVWorldHelperLifecycle helperLifecycle = new OldVWorldHelperLifecycle();

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
