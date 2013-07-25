package org.friendularity.spec.connection;

import java.util.HashMap;
import java.util.Map;
import org.appdapter.core.component.KnownComponentImpl;
import org.jflux.api.registry.Descriptor;
import org.jflux.api.registry.basic.BasicDescriptor;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceDependency.Cardinality;
import org.jflux.api.service.ServiceDependency.UpdateStrategy;
import org.jflux.api.service.binding.ServiceBinding.BindingStrategy;

/**
 * The data object for the dependency binding properties of a service.
 *
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class ServiceBindingSpec extends KnownComponentImpl {
    // The stored data definitions
    
    private String myClassName;
    private Map<String, String> myProperties;
    private Cardinality myCardinality;
    private UpdateStrategy myUpdateStrategy;
    
    private BindingStrategy myBindingStrategy;
    
    public ServiceBindingSpec() {
        myProperties = new HashMap<String, String>();
        myCardinality = null;
        myUpdateStrategy = null;
    }
    
    // Getters for data
    
    public ServiceDependency getServiceDependency() {
        return new ServiceDependency(
                myClassName, myClassName, myCardinality, myUpdateStrategy,
                myProperties);
    }
    
    public Descriptor getDescriptor() {
        return new BasicDescriptor(myClassName, myProperties);
    }
    
    public BindingStrategy getBindingStrategy() {
        return myBindingStrategy;
    }
    
    // Setters for data
    
    public void setClassName(String className) {
        myClassName = className;
    }
    
    public void setBindingStrategy(BindingStrategy bindingStrategy) {
        myBindingStrategy = bindingStrategy;
    }
    
    public void setCardinality(Cardinality cardinality) {
        myCardinality = cardinality;
    }
    
    public void setUpdateStrategy(UpdateStrategy updateStrategy) {
        myUpdateStrategy = updateStrategy;
    }
    
    // Accumulators for data
    
    public void addProperty(String key, String value) {
        myProperties.put(key, value);
     }
     
    public void removeProperty(String key) {
        myProperties.remove(key);
    }
}
