/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.scripting;

/**
 *
 * @author Administrator
 */
public class ScriptArg extends Number {

    String value;

    public ScriptArg(String s) {
        value = s;
    }

    public int intValue() {
        return Integer.parseInt(value);
    }

    @Override
    public long longValue() {
        return Long.parseLong(value);
    }

    @Override
    public float floatValue() {
        return Float.parseFloat(value);
    }

    @Override
    public double doubleValue() {
        return Double.parseDouble(value);
    }

    public String stringValue() {
        return value;
    }
}
