package org.friendularity.spec.connection;

import org.appdapter.core.component.KnownComponentImpl;
import org.robokind.impl.messaging.utils.ConnectionUtils;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class DestinationSpec extends KnownComponentImpl {
    private String myName;
    private int myType;
    
    public DestinationSpec() {
    }
    
    public String getName() {
        return myName;
    }
    
    public void setName(String name) {
        myName = name;
    }
    
    public int getType() {
        return myType;
    }
    
    public void setType(int type) {
        if(type != ConnectionUtils.QUEUE && type != ConnectionUtils.TOPIC) {
            throw new IllegalArgumentException(
                    "Destination type must be either a queue or a topic");
        }
    }
}
