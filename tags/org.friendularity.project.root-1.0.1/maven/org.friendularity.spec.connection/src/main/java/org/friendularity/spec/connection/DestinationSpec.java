package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl;
import org.robokind.impl.messaging.utils.ConnectionUtils;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class DestinationSpec extends KnownComponentImpl {
    public final static String QUEUE_TYPE =
            "http://www.friedularity.org/Connection#amqpQueue";
    public final static String TOPIC_TYPE =
            "http://www.friedularity.org/Connection#amqpTopic";
    
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
        
        myType = type;
    }
    
    public void setType(String type) {
        if(type.equals(QUEUE_TYPE)) {
            myType = ConnectionUtils.QUEUE;
        } else if(type.equals(TOPIC_TYPE)) {
            myType = ConnectionUtils.TOPIC;
        } else {
            throw new IllegalArgumentException(
                    "Destination type must be either a queue or a topic");
        }
    }
}
