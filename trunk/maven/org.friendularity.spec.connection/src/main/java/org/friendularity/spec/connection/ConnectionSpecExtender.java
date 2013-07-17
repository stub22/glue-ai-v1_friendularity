/*
 * Copyright 2013 The Friendularity Project (www.friendularity.org).
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
package org.friendularity.spec.connection;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.jflux.api.registry.Registry;
import org.jflux.api.service.ServiceManager;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.osgi.ServiceClassListener;

/**
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionSpecExtender extends ServiceClassListener<ConnectionSpec>{
    /**
     * Stores the managed connections for later removal
     */
    private Map< ConnectionSpec, ServiceManager> myManagedConnectionsMap;
    /**
     * Context reference for interacting with JFlux
     */
    private Registry myRegistry;
    
    public ConnectionSpecExtender(BundleContext context, Registry registry, String serviceFilter)  {
        super(ConnectionSpec.class, context, serviceFilter);
        myRegistry = registry;
        myManagedConnectionsMap = new HashMap<ConnectionSpec, ServiceManager>();
    }
    
    /**
     * Create and register the connection.
     * @param connectionSpec data object used to generate connection
     */
    @Override
    protected void addService(ConnectionSpec connectionSpec) {
        if( connectionSpec == null || myManagedConnectionsMap.containsKey(connectionSpec) ) {
            return;
        }
        ConnectionLifecycle lifecycle = new ConnectionLifecycle(connectionSpec);
        // As this class has no dependancies, an empty collection is sufficient for JFlux
        ServiceManager managedConnection = new ServiceManager(lifecycle, Collections.EMPTY_MAP, Collections.EMPTY_MAP, null);
        // Start the service manager which will create and register an Connection instance
        managedConnection.start(myRegistry);
        // Store the connection so it may be removed later.
        myManagedConnectionsMap.put(connectionSpec, managedConnection);
    }
    
    /**
     * Removes the connection that the given spec created from the JFlux
     * registry
     * @param connectionSpec  data object used to generate connection
     */
    @Override
    protected void removeService(ConnectionSpec connectionSpec) {
        if(connectionSpec == null || !myManagedConnectionsMap.containsKey(connectionSpec)) {
            return;
        }
        ServiceManager manager = myManagedConnectionsMap.remove(connectionSpec);
        if(manager != null){
            manager.stop();
        }
    }
}
