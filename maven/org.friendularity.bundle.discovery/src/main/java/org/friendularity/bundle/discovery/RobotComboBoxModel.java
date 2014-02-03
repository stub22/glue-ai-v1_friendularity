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

package org.friendularity.bundle.discovery;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.DefaultComboBoxModel;
import org.jflux.api.core.Listener;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class RobotComboBoxModel extends DefaultComboBoxModel
    implements Listener<RobotService> {
    private Map<String, String> myRobotServiceMap;
    private List<RobotService> myRobots;
    private String mySelectedItem;
    
    public RobotComboBoxModel(Discoverer discoverer) {
        myRobotServiceMap = new HashMap<String, String>();
        myRobots = new ArrayList<RobotService>();
        mySelectedItem = null;
        discoverer.addListener(this);
    }
    
    @Override
    public void setSelectedItem(Object o) {
        for(RobotService robot: myRobots) {
            if(robot.getSerial().equals(o.toString())) {
                mySelectedItem = robot.getSerial();
            }
        }
    }
    
    @Override
    public Object getSelectedItem() {
        return mySelectedItem;
    }
    
    @Override
    public int getSize() {
        return myRobots.size();
    }
    
    @Override
    public Object getElementAt(int i) {
        if(myRobots.isEmpty()) {
            return null;
        } else {
            return myRobots.get(i).getSerial();
        }
    }
    
    public String getSelectedIP() {
        return myRobotServiceMap.get(mySelectedItem);
    }

    @Override
    public void handleEvent(RobotService t) {
        String serial = t.getSerial();
        String ipAddress = t.getIPAddress();
        
        if(myRobotServiceMap.containsKey(serial) &&
                myRobotServiceMap.get(serial).equals(ipAddress)) {
            return;
        }
        
        if(myRobotServiceMap.containsKey(serial)) {
            // This means the IP changed.
            
            int index = 0;
            
            for(int i = 0; i < myRobots.size(); i++) {
                if(myRobots.get(i).getSerial().equals(serial)) {
                    index = i;
                    break;
                }
            }
            
            myRobots.remove(index);
            myRobots.add(index, t);
        } else {
            // We have a new robot.
            
            myRobots.add(t);
        }

        myRobotServiceMap.put(serial, ipAddress);
        fireContentsChanged(this, 0, myRobots.size() - 1);
    }
    
}
