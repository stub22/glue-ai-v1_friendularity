/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.scripting;

/**
 *
 * @author Douglas
 */
abstract public class CogbotPrimitive {


    String name;
    int arity;
    
    protected CogbotPrimitive(String named, int args) {
        name = named;
        arity = args;
    }

    public String getName() {
        return name;
    }

    public int getArity() {
        return arity;
    }

    public abstract Object execute(ScriptArg[] scriptArgs);
}
