/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.config;

/**
 *
 * @author matt
 */
public class EmptyFormatter implements IStringFormatter{
    public String format(String input){
        return input;
    }
}
