/*
 * Copyright 2014 Hanson Robokind LLC.
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

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class RobotService {
    private String myIPAddress;
    private String mySerial;
    
    public RobotService(String ipAddress, String serial) {
        myIPAddress = ipAddress;
        mySerial = serial;
    }
    
    public String getIPAddress() {
        return myIPAddress;
    }
    
    public String getSerial() {
        return mySerial;
    }
    
    @Override
    public boolean equals(Object other) {
        if(other instanceof RobotService) {
            RobotService oService = (RobotService)other;
            
            return oService.getIPAddress().equals(myIPAddress) &&
                    oService.getSerial().equals(mySerial);
        }
        
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 47 * hash +
                (this.myIPAddress != null ? this.myIPAddress.hashCode() : 0);
        hash = 47 * hash +
                (this.mySerial != null ? this.mySerial.hashCode() : 0);
        return hash;
    }
}
