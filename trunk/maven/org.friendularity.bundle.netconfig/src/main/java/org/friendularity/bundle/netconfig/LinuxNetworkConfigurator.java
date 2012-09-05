/*
 * Copyright 2012 Hanson Robokind LLC.
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

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class LinuxNetworkConfigurator implements NetworkConfigurator {
    private final static Logger theLogger =
            Logger.getLogger(LinuxNetworkConfigurator.class.getName());
    
    public LinuxNetworkConfigurator() {
        
    }
    
    @Override
    public void configureNetwork(NetworkConfig config) {
        Map<String, Map<String, String>> confMap = createMap(config);
        String confData = makeConfFile(confMap);
        
        writeConfig(confData, "/etc/NetworkManager/system-connections/nm_conf");
        
        restartServices();
    }
    
    private Map<String, Map<String, String>> createMap(NetworkConfig config) {
        Map<String, Map<String, String>> confMap =
                new HashMap<String, Map<String, String>>();
        Map<String, String> connectionMap = new HashMap<String, String>();
        Map<String, String> wirelessMap = new HashMap<String, String>();
        Map<String, String> ipv4Map = new HashMap<String, String>();
        Map<String, String> ipv6Map = new HashMap<String, String>();
        Map<String, String> securityMap;
        
        confMap.put("connection", connectionMap);
        confMap.put("802-11-wireless", wirelessMap);
        confMap.put("ipv4", ipv4Map);
        confMap.put("ipv6", ipv6Map);
        
        connectionMap.put("type", "802-11-wireless");
        connectionMap.put("id", "nm_conf");
        connectionMap.put("autoconnect", "true");
        
        wirelessMap.put("mode", "infrastructure");
        wirelessMap.put("ssid", makeSsid(config.getSsid()));
        
        ipv4Map.put("method", "auto");
        
        ipv6Map.put("method", "ignore");
        
        if(config.getSecurity() != WiFiSecurity.NONE) {
            securityMap = new HashMap<String, String>();
            
            confMap.put("802-11-wireless-security", securityMap);
            
            wirelessMap.put("security", "802-11-wireless-security");
            
            if(config.getSecurity() == WiFiSecurity.WPA) {
                securityMap.put("key-mgmt", "wpa-psk");
                //securityMap.put("wep-tx-keyidx", "0");
                securityMap.put("psk", config.getKey());
                //securityMap.put("wep-key-type", "0");
            }
            else if(config.getSecurity() == WiFiSecurity.WEP) {
                securityMap.put("key-mgmt", "none");
                securityMap.put("wep-tx-keyidx", "0");
                securityMap.put("auth-alg", "open");
                securityMap.put("wep-key0", config.getKey());
                securityMap.put("wep-key-type", "1");
            }
        }
        
        return confMap;
    }
    
    private String makeSsid(String textSsid) {
        StringBuilder ssid = new StringBuilder();
        
        for(char i: textSsid.toCharArray()) {
            ssid.append(Character.getNumericValue(i)).append(";");
        }
        
        return ssid.toString();
    }
    
    private String makeConfFile(Map<String, Map<String, String>> confMap) {
        StringBuilder confData = new StringBuilder();
        
        for(String i: confMap.keySet()) {
            Map<String, String> sectionMap = confMap.get(i);
            confData.append("[").append(i).append("]\n");
            
            for(String j: sectionMap.keySet()) {
                String k = sectionMap.get(j);
                confData.append(j).append("=").append(k).append("\n");
            }
            
            confData.append("\n");
        }
        
        return confData.toString();
    }
    
    private void writeConfig(String confData, String fileName) {
        try {
            BufferedWriter writer =
                    new BufferedWriter(new FileWriter(fileName));
            writer.write(confData);
            writer.close();
        }
        catch(Exception e) {
            theLogger.log(
                    Level.SEVERE, "Failed to write to file {0}", fileName);
        }
    }
    
    private void restartServices() {
        runCommand("sudo service network-manager stop");
        
        runCommand("sudo killall NetworkManager");
        runCommand("sudo killall dhclient");
        runCommand("sudo killall dhclient3");
        runCommand("sudo killall wpa_supplicant");
        
        runCommand("sudo service network-manager start");
    }
    
    private int runCommand(String cmd) {
        try {
            Process command = Runtime.getRuntime().exec(cmd);
            int retVal = command.waitFor();
            
            return retVal;
        }
        catch(Exception e) {
            theLogger.log(Level.SEVERE, "Failed to run {0}", cmd);
            
            return 1;
        }
    }
}
