package org.friendularity.spec.connection;

import org.appdapter.core.component.KnownComponentImpl;

/**
 * The data object for a property.
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class PropertySpec extends KnownComponentImpl {
    // The stored data definitions
    
    private String myName;
    private String myValue;
    
    // Getters for data
    
    public String getName() {
        return myName;
    }
    
    public String getValue() {
        return myValue;
    }
    
    // Setters for data
    
    public void setName(String name) {
        myName = name;
    }
    
    public void setValue(String value) {
        myValue = value;
    }
}
