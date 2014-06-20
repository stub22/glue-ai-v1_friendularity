/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.cogbot.scripting;

import java.lang.reflect.Method;
import java.util.HashMap;

/**
 *
 *   Right now this doesnt do much .. but deciding if the methods it uses will be choosen by a config
 *
 * @author Administrator
 */
public class ClassLispWriter {
    private static HashMap<Class,ClassLispWriter> toSExprClass = new HashMap<Class, ClassLispWriter>();
    
    static ClassLispWriter getClassInfo(Class c) {
        ClassLispWriter lc = toSExprClass.get(c);
        if (lc==null) {
            lc = new ClassLispWriter(c);
            toSExprClass.put(c,lc);
        }
        return lc;
    }
    private final Class clazz;

    private ClassLispWriter(Class c) {
       this.clazz = c;
    }

    public Method[] getMethods() {
       return clazz.getMethods();
    }

}
