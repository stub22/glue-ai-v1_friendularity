/*
 * Copyright 2014 the Friendularity Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.friendularity.bundle.ui.swing.animation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.prefs.Preferences;
import javax.swing.DefaultComboBoxModel;
import org.apache.commons.lang.StringUtils;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class PlayerClientComboBoxModel extends DefaultComboBoxModel {
    private List<String> myEntries;
    private String selected;
    private Preferences myPrefs;
    
    public PlayerClientComboBoxModel() {
        myPrefs = Preferences.userRoot().node("org.mechio.demo.animation");
        String histString = myPrefs.get("ip_history", "127.0.0.1");
        String[] history = histString.split("¤");
        myEntries = new ArrayList<String>();
        myEntries.addAll(Arrays.asList(history));
        
        if(!myEntries.isEmpty()) {
            selected = myEntries.get(0);
        } else {
            selected = null;
        }
    }

    @Override
    public void setSelectedItem(Object anItem) {
        if(!myEntries.contains(anItem.toString())) {
            myEntries.add(anItem.toString());
            
            String history = StringUtils.join(myEntries, "¤");
            myPrefs.put("ip_history", history);
        }
        
        selected = anItem.toString();
        
        fireContentsChanged(this, 0, myEntries.size() - 1);
    }

    @Override
    public Object getSelectedItem() {
        return selected;
    }

    @Override
    public int getSize() {
        return myEntries.size();
    }

    @Override
    public Object getElementAt(int index) {
        if(myEntries.isEmpty()) {
            return null;
        } else {
            return myEntries.get(index);
        }
    }
    
    public void clear() {
        myEntries = new ArrayList<String>();
        selected = "";
        myPrefs.put("ip_history", "");
    }
}
