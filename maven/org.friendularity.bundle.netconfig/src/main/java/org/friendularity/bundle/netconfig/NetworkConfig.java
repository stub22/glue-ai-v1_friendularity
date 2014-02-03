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

package org.friendularity.bundle.netconfig;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class NetworkConfig {
    private String mySsid;
    private WiFiSecurity mySecurity;
    private String myKey;
    
    public NetworkConfig(String ssid, WiFiSecurity security, String key) {
        mySsid = ssid;
        mySecurity = security;
        myKey = key;
    }
    
    public NetworkConfig(String ssid) {
        mySsid = ssid;
        mySecurity = WiFiSecurity.NONE;
        myKey = "";
    }
    
    public String getSsid() {
        return mySsid;
    }
    
    public WiFiSecurity getSecurity() {
        return mySecurity;
    }
    
    public String getKey() {
        return myKey;
    }
}
