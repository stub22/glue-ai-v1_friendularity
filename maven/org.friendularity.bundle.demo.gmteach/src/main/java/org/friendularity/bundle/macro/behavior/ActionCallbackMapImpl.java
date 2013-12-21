/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.behavior;

import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.cogchar.svc.behav.control.ActionCallbackMap;
import org.jflux.api.core.Listener;
import org.jflux.api.core.Notifier;
import org.jflux.api.core.util.DefaultNotifier;

/**
 *
 * @author matt
 */
public class ActionCallbackMapImpl implements ActionCallbackMap {

    public final static String PROP_TRIGGER_PANEL_ID = "triggerPanelID";
    private Map<String, ActionListener> myButtonMap = new HashMap<String, ActionListener>();
    private Notifier<String> myActionAddedNotifier;
    private Notifier<String> myActionRemovedNotifier;

    public ActionCallbackMapImpl() {
        myActionAddedNotifier = new DefaultNotifier<String>();
        myActionRemovedNotifier = new DefaultNotifier<String>();
    }

    @Override
    public void putActionCallback(String name, ActionListener listener) {
        myButtonMap.put(name, listener);
        myActionAddedNotifier.notifyListeners(name);
    }

    @Override
    public void removeActionCallback(String name) {
        myButtonMap.remove(name);
        myActionRemovedNotifier.notifyListeners(name);
    }

    @Override
    public ActionListener getActionCallback(String actionCallbackName) {
        return myButtonMap.get(actionCallbackName);
    }

    public void addActionListener(Listener<String> listener) {
        myActionAddedNotifier.addListener(listener);
    }

    public void removeActionListener(Listener<String> listener) {
        myActionAddedNotifier.removeListener(listener);
    }

    public void addActionRemoveListener(Listener<String> listener) {
        myActionRemovedNotifier.addListener(listener);
    }

    public void removeActionRemoveListener(Listener<String> listener) {
        myActionRemovedNotifier.removeListener(listener);
    }

    public Set<String> getActionKeys() {
        return myButtonMap.keySet();
    }
}
