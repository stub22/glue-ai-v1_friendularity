package org.friendularity.spec.connection;

import java.util.HashMap;
import java.util.Map;
import org.appdapter.core.component.KnownComponentImpl;

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
    
    public ServiceBindingSpec() {
        myProperties = new HashMap<String, String>();
    }
    
    // Getters for data
    
    public String getClassName() {
        return myClassName;
    }
    
    public Map<String, String> getProperties() {
        return myProperties;
    }
    
    // Setters for data
    
    public void setClassName(String className) {
        myClassName = className;
    }
    
    // Accumulators for data
    
    public void addProperty(String key, String value) {
        myProperties.put(key, value);
    }
    
    public void removeProperty(String key) {
        myProperties.remove(key);
    }
}
