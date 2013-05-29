/*
 *  Copyright 2012 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.api.connection;

import org.appdapter.core.component.KnownComponentImpl;


/**
 * Data Object representing a ConnectionSpec
 * 
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionSpec extends KnownComponentImpl {
    private String ipAddress;
    private String port;
    private String username;
    private String password;
    private String clientName;
    private String virtualHost;
    private String connectionOptions;

    public ConnectionSpec(String ipAddress, String port, String username, String password, String clientName, String virtualHost, String connectionOptions) {
        this.ipAddress = ipAddress;
        this.port = port;
        this.username = username;
        this.password = password;
        this.clientName = clientName;
        this.virtualHost = virtualHost;
        this.connectionOptions = connectionOptions;
    }

    public String getIpAddress() {
        return ipAddress;
    }

    public void setIpAddress(String ipAddress) {
        this.ipAddress = ipAddress;
    }

    public String getPort() {
        return port;
    }

    public void setPort(String port) {
        this.port = port;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getClientName() {
        return clientName;
    }

    public void setClientName(String clientName) {
        this.clientName = clientName;
    }

    public String getVirtualHost() {
        return virtualHost;
    }

    public void setVirtualHost(String virtualHost) {
        this.virtualHost = virtualHost;
    }

    public String getConnectionOptions() {
        return connectionOptions;
    }

    public void setConnectionOptions(String connectionOptions) {
        this.connectionOptions = connectionOptions;
    }
}