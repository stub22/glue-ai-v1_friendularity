/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.demo.lifecycles;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.jflux.api.service.DependencySpec;
import org.jflux.api.service.ServiceLifecycle;
import org.robokind.api.speech.viseme.VisemeBindingManager;
import org.robokind.api.speech.viseme.config.VisemeBindingManagerConfig;
import org.robokind.api.speech.viseme.config.VisemeBindingManagerFactory;

/**
 *
 * @author eadsjr
 */
public class VisemeBindingManagerLifecycle implements ServiceLifecycle<VisemeBindingManager> {

    private final static String theVisemeConfig = "visemeManagerConfig";
    
    private final static List<DependencySpec> theDependencySpecs = new ArrayList<DependencySpec>(Arrays.asList(
            new DependencySpec(theVisemeConfig, VisemeBindingManagerConfig.class.getName(), DependencySpec.Cardinality.MANDATORY_UNARY, DependencySpec.UpdateStrategy.DYNAMIC, null)));
 
    
    private final static String[] theClassNames = new String[]{VisemeBindingManager.class.getName()};
    
    public List<DependencySpec> getDependencySpecs() {
        return theDependencySpecs;
    }

    public VisemeBindingManager createService(Map<String, Object> services) {
        VisemeBindingManagerConfig conf = 
                (VisemeBindingManagerConfig)services.get(theVisemeConfig);
        return VisemeBindingManagerFactory.buildManager(conf);
    }

    public VisemeBindingManager handleDependencyChange(VisemeBindingManager t, String changeType,
            String dependencyName, Object dependency, Map<String,Object> availableDependencies) {
        
        return VisemeBindingManagerFactory.buildManager((VisemeBindingManagerConfig)t);
    }

    public void disposeService(VisemeBindingManager t, Map<String, Object> map) {
    }

    public String[] getServiceClassNames() {
        return theClassNames;
    }
    
}
