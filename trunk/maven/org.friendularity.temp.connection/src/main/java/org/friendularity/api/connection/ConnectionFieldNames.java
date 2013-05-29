/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

/**
 * This provides the names to be used in retrieving a ConnectionSpec from the 
 * data source in Appdapter.
 * 
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionFieldNames {
    public static String        PATH_ipAddress = "ipAddress";
    public static String        PATH_port = "port";
    public static String        PATH_username = "username";
    public static String        PATH_password = "password";
    public static String        PATH_clientName = "clientName";
    public static String        PATH_virtualHost = "virtualHost";
    public static String        PATH_connectionOptions = "connectionOptions";
}
