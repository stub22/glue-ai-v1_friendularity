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

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;
import org.robokind.api.common.utils.TimeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author jgpallack
 */
public class Beacon implements Runnable {
    private final static String theSsdpAddr = "239.255.255.250";
    private final static int theSsdpPort = 1900;
    private final static Logger theLogger =
            LoggerFactory.getLogger(Beacon.class);
    
    public static void main(String[] args) {
        Thread thread = new Thread(new Beacon());
        thread.start();
    }
    
    @Override
    public void run() {
        InetAddress group;
        MulticastSocket sock;
        
        try {
            group = InetAddress.getByName(theSsdpAddr);
        } catch(UnknownHostException ex) {
            theLogger.error(
                    "Unknown host " + theSsdpAddr + ": " + ex.getMessage());
            return;
        }
        
        try {
            sock = new MulticastSocket(theSsdpPort);
            sock.joinGroup(group);
        } catch(IOException ex) {
            theLogger.error("Error creating socket: " + ex.getMessage());
            return;
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("NOTIFY * HTTP/1.1\r\n");
        sb.append("HOST: 239.255.255.250:1900\r\n");
        sb.append("NTS: ssdp:alive\r\n");
        sb.append("EXT: \r\n");
        sb.append("USN: R25\r\n");
        sb.append("CACHE-CONTROL: max-age=1800\r\n");
        sb.append("SERVER: Linux,");
        sb.append(System.getProperty("os.version"));
        sb.append(",GLUE-AI\r\n");
        sb.append("LOCATION: zeno-r25-000002\r\n");
        sb.append("NT: upnp:rootdevice\r\n");
        String http = sb.toString();
        
        while(true) {
            DatagramPacket message =
                    new DatagramPacket(
                            http.getBytes(), http.length(), group, 1900);
            try {
                sock.send(message);
            } catch(IOException ex) {
                theLogger.error("Error sending message: " + ex.getMessage());
            }
            
            TimeUtils.sleep(10000);
        }
    }
   
}
