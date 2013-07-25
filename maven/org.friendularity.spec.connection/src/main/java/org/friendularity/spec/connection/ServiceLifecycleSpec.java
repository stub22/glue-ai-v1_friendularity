package org.friendularity.spec.connection;

import org.appdapter.core.component.KnownComponentImpl;
import org.jflux.api.service.ServiceLifecycle;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class ServiceLifecycleSpec extends KnownComponentImpl {
    private Class myLifecycleClass;
    
    public Class getLifecycleClass() {
        return myLifecycleClass;
    }
    
    public void setLifecycleClass(Class cls) {
        myLifecycleClass = cls;
    }
}
