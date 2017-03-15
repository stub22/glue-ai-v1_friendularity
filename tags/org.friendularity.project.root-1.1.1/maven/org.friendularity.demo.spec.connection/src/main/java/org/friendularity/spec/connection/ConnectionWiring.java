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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import org.appdapter.core.name.Ident;
import org.osgi.framework.BundleContext;
import org.appdapter.xload.rspec.OnlineSheetRepoSpec;
import org.appdapter.fancy.rclient.EnhancedRepoClient;
import org.jflux.api.registry.Registry;
import org.jflux.impl.registry.OSGiRegistry;
import org.jflux.impl.services.rk.lifecycle.ManagedService;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;

/**
 *
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionWiring {

    public static final String GROUP_KEY_FOR_CONNECION_SPEC = "connectionSpecGroupId";
    public static final String CONNECTION_GROUP_QN = "demoConnectionGroup";
    
    public static ConnectionSpecExtender startSpecExtender(BundleContext bundleCtx, String optionalSpecFilter) {
        Registry reg = new OSGiRegistry(bundleCtx);
        ConnectionSpecExtender cse = new ConnectionSpecExtender(bundleCtx, reg, optionalSpecFilter);
        cse.start();
        return cse;
    }
    
    public static Map<ConnectionSpec,ManagedService> loadAndRegisterSpecs(
            BundleContext context, EnhancedRepoClient defaultDemoRepoClient, String connectionGraphQN) {
        Map<ConnectionSpec,ManagedService> specServices = new HashMap<ConnectionSpec, ManagedService>();
        List<ConnectionSpec> specs = loadConnectionSpecs(defaultDemoRepoClient, connectionGraphQN);
        for(ConnectionSpec spec : specs){
            if(specServices.containsKey(spec)){
                continue;
            }
            ManagedService service = registerConnectionSpec(context, spec);
            specServices.put(spec, service);
        }
        return specServices;
    }
    
    private static List<ConnectionSpec> loadConnectionSpecs(EnhancedRepoClient defaultDemoRepoClient, String connectionGraphQN) {
        List<ConnectionSpec> specs = new ArrayList();
        // Determine the URI for the 'qualified name' which identifies the data in the repo
        Ident connectionGraphID = defaultDemoRepoClient.getDefaultRdfNodeTranslator().makeIdentForQName(connectionGraphQN);
        // Collect the objects from the repo, building them from RDF raw data
        Set<Object> assembledRoots = defaultDemoRepoClient.assembleRootsFromNamedModel(connectionGraphID);
        for (Object root : assembledRoots) {
            // Ignore anything that is not a ConnectionSpec
            if (root == null || !ConnectionSpec.class.isAssignableFrom(root.getClass())) {
                continue;
            }
            specs.add((ConnectionSpec) root);
        }
        return specs;
    }

    /**
     * Allows JFlux to register connection specs
     * @param context the BundleContext used to register the spec
     * @param connectionSpec the spec to be registered
     */
    private static ManagedService registerConnectionSpec(
            BundleContext context, ConnectionSpec connectionSpec) {
        Properties props = new Properties();
        props.put(GROUP_KEY_FOR_CONNECION_SPEC, CONNECTION_GROUP_QN);
        
        ServiceLifecycleProvider lifecycle = 
                new SimpleLifecycle(connectionSpec, ConnectionSpec.class);
        ManagedService ms = new OSGiComponent(context, lifecycle, props);
        ms.start();
        return ms;
    }
}