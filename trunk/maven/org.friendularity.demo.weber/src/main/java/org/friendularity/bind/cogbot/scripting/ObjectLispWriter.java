/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.scripting;



import java.lang.reflect.Method;
import java.util.IdentityHashMap;
import javax.management.AttributeChangeNotification;
import org.cogchar.api.integroid.cue.PersonCue;
import org.cogchar.api.platform.cues.NamedCue;
import org.cogchar.platform.stub.ThalamentStub;

/**
 *
 * @author Administrator
 */
public class ObjectLispWriter {

    final static Object[] OBJS0 = new Object[0];

    static String makeSObject(Object target, IdentityHashMap<Object, String> excptFor) {
        
        String found = objectSimpleString(excptFor, target);
        if (found != null) {
            return found;
        }

        found = makeObjRef(excptFor, target);
        excptFor.put(target, found);
        found = expandObj(target, excptFor);
        if (found == null) {
            found = expandClassObj(excptFor, target);
        }
        return found;
    }

    static String expandObj(Object target, IdentityHashMap<Object, String> excptFor) {
        if (target instanceof AttributeChangeNotification) {
            AttributeChangeNotification acn = (AttributeChangeNotification) target;
            String attribName = acn.getAttributeName();
            String attribTypeName = acn.getAttributeType();
            Object newValue = acn.getNewValue();
            Object oldValue = acn.getOldValue();
            String nvs = makeSObject(newValue, excptFor);
            String ovs = nvs;
            if (newValue != oldValue) {
                ovs = makeSObject(oldValue, excptFor);
            }
            return makeList(excptFor, "AttributeChangeNotification", attribName, quotedString(attribTypeName), ovs, nvs);
        }
        
        /*
        String targetName = makeObjRef(excptFor, target);

         if (target instanceof VariableCue) {
            VariableCue o = (VariableCue) target;
            return makeList(excptFor, targetName, o.getValue(), o.getSimpleClassName(), o.getStrength(),
                    o.getScheduledExpireTimeSec(), o.getUpdateStampMsec());
        }

        if (target instanceof TextCue) {
            TextCue o = (TextCue) target;
            return makeList(excptFor, targetName, o.getTextData(), o.getSimpleClassName(), o.getStrength(),
                    o.getScheduledExpireTimeSec(), o.getUpdateStampMsec());
        }
        if (target instanceof NamedCue) {
            NamedCue o = (NamedCue) target;
            return makeList(excptFor, targetName, o.getContentSummaryString(), o.getSimpleClassName(), o.getStrength(),
                    o.getScheduledExpireTimeSec(), o.getUpdateStampMsec());
        }

        if (target instanceof Cue) {
            Cue o = (Cue) target;
            return makeList(excptFor, targetName, target.toString(), o.getSimpleClassName(), o.getStrength(),
                    o.getScheduledExpireTimeSec(), o.getUpdateStampMsec());
        }


        if (target instanceof StepJob) {
            Thalament o = (Thalament) target;
            return makeList(excptFor, targetName, target.toString(), o.getSimpleClassName(), 1.0,
                    -1, o.getUpdateStampMsec());
        }

        if (target instanceof Thalament) {
            Thalament o = (Thalament) target;
            return makeList(excptFor, targetName, target.toString(), o.getSimpleClassName(), 1.0,
                    -1, o.getUpdateStampMsec());
        }
*/
        return null;
    }

    static String nv(String n, String v) {
        return "(" + n + " . " + v + ")";
    }

    static String makeList(IdentityHashMap<Object, String> excptFor, String a1, Object... obj) {
        StringBuffer sb = new StringBuffer("(" + a1);
        for (Object o : obj) {
            sb.append(" ");
            String found;
            if (o instanceof String) {
                found = (String) o;
            } else {
                found = makeObjRef(excptFor, o);
            }
            sb.append(found);
        }
        sb.append(")");
        return sb.toString();
    }

    static String makeObjRef(IdentityHashMap<Object, String> excptFor, Object obj) {

        String found = objectSimpleString(excptFor, obj);
        if (found != null) {
            return found;
        }

        Class c = obj.getClass();
        final int hc = obj.hashCode();
        final int sihc = System.identityHashCode(obj);
        int uhc = sihc;

//        found = "" + obj;


        
        if (hc != sihc) {
            if (hc < -1 || hc > 1) {
                uhc = hc;
            }
            if (obj instanceof Comparable) {
            }
        }

        if (obj instanceof NamedCue) {
            return ((NamedCue) obj).getName();
        }
        if (obj instanceof PersonCue) {
            PersonCue o = (PersonCue) obj;
            return o.getTypeString() + o.getThalamentID();
        }
        if (obj instanceof ThalamentStub) {
            ThalamentStub o = (ThalamentStub) obj;
            return o.getTypeString() + o.getThalamentID();
        }
        return makeList(excptFor, "JavaObject", objectSimpleString(excptFor, c), sihc);
    }

    static String expandClassObj(IdentityHashMap<Object, String> excptFor, Object target) {
        Class c = target.getClass();
        ClassLispWriter lc = ClassLispWriter.getClassInfo(c);
        StringBuffer sb = new StringBuffer();
        sb.append("(" + makeObjRef(excptFor, c) + " ");
        for (Method m : lc.getMethods()) {
            try {
                if (m.getParameterTypes().length != 0) {
                    continue;
                }
                if (m.getReturnType() == void.class) {
                    continue;
                }
                String mn = m.getName();
                String propname;
                if (mn.startsWith("is")) {
                    propname = mn.substring(2);
                } else if (mn.startsWith("get")) {
                    propname = mn.substring(3);
                } else {
                    continue;
                }
                if (propname.length() < 3) {
                    continue;
                }
                char ch = propname.charAt(0);
                if (!Character.isUpperCase(ch)) {
                    continue;
                }
                if (!Character.isLetter(ch)) {
                    continue;
                }
                Object os = m.invoke(target, OBJS0);
                sb.append(" " + nv(propname, makeSObject(os, excptFor)));
            } catch (Throwable ex) {
                //Logger.getLogger(ObjectLispWriter.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        sb.append(")");
        return sb.toString();
    }

    public static String makeLispObject(Object acn) {

        IdentityHashMap<Object, String> circles = new IdentityHashMap<Object, String>();
        return makeSObject(acn, circles);
    }

    private static String objectSimpleString(IdentityHashMap<Object, String> excptFor, Object obj) {
        if (obj == null) {
            return "JavaNull";
        }

        String found = excptFor.get(obj);
        if (found != null) {
            return found;
        }

        if (obj instanceof Number) {
            return obj.toString();
        }

        if (obj instanceof String) {
            found = (String) obj;
            return quotedString(found);
        }
        
        if (obj instanceof Class) {
            Class o = (Class) obj;
            found = o.getSimpleName();
            if (found != null && found.length() > 0) {
                return found;
            }
            return "" + o;
        }
        
        return null;
    }

    private static String quotedString(String found) {
        if (found==null) return "JavaNull";
        return "\"" + found.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }
}
