package org.friendularity.spec.connection;

import org.appdapter.core.component.KnownComponentImpl;
import org.jflux.api.service.ServiceDependency.Cardinality;
import org.jflux.api.service.ServiceDependency.UpdateStrategy;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class ServiceLifecycleSpec extends KnownComponentImpl {
    private String myLifecycleClassName;
    private Cardinality myCardinality;
    private UpdateStrategy myUpdateStrategy;
    
    public String getLifecycleClassName() {
        return myLifecycleClassName;
    }
    
    public Cardinality getCardinality() {
        return myCardinality;
    }
    
    public UpdateStrategy getUpdateStrategy() {
        return myUpdateStrategy;
    }
    
    public void setLifecycleClassName(String className) {
        myLifecycleClassName = className;
    }
    
    public void setCardinality(Cardinality cardinality) {
        myCardinality = cardinality;
    }
    
    public void setUpdateStrategy(UpdateStrategy updateStrategy) {
        myUpdateStrategy = updateStrategy;
    }
}
