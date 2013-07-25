package org.friendularity.spec.connection;

import org.appdapter.core.component.KnownComponentImpl;
import org.jflux.api.service.ServiceDependency.Cardinality;
import org.jflux.api.service.ServiceDependency.UpdateStrategy;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class ServiceLifecycleSpec extends KnownComponentImpl {
    private Class myLifecycleClass;
    private Cardinality myCardinality;
    private UpdateStrategy myUpdateStrategy;
    
    public Class getLifecycleClass() {
        return myLifecycleClass;
    }
    
    public Cardinality getCardinality() {
        return myCardinality;
    }
    
    public UpdateStrategy getUpdateStrategy() {
        return myUpdateStrategy;
    }
    
    public void setLifecycleClass(Class cls) {
        myLifecycleClass = cls;
    }
    
    public void setCardinality(Cardinality cardinality) {
        myCardinality = cardinality;
    }
    
    public void setUpdateStrategy(UpdateStrategy updateStrategy) {
        myUpdateStrategy = updateStrategy;
    }
}
