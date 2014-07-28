package org.friendularity.bundle.discovery;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class SerialNumberLifecycle
    implements ServiceLifecycle<SerialNumberSpec> {
    private final static String theSpecName = "serialNumberSpec";
    
    private final static ServiceDependency[] theDependencyArray = {
        new ServiceDependency(theSpecName, SerialNumberSpec.class.getName(),
        ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };
    
    private final static String[] theClassNameArray = {
        SerialNumberSpec.class.getName()
    };

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public SerialNumberSpec createService(Map<String, Object> map) {
        return (SerialNumberSpec)map.get(theSpecName);
    }

    @Override
    public SerialNumberSpec handleDependencyChange(
            SerialNumberSpec t, String string, String string1, Object o,
            Map<String, Object> map) {
        return null;
    }

    @Override
    public void disposeService(SerialNumberSpec t, Map<String, Object> map) {
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
}
