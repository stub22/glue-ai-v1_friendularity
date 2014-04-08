package org.friendularity.bind.js;

/**
 * Provides a control object for javascript code.
 *
 * @author Jason Randolph Eads <jeads362@gmail.com>
 */
public class ControlSystem {
    boolean myExitingNow;
    
    public ControlSystem() {
        myExitingNow = true;
    }

    public synchronized boolean isExitingNow() {
        return myExitingNow;
    }

    public synchronized void setExitingNow(boolean exitingNow) {
        myExitingNow = exitingNow;
    }
}
