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
import java.util.HashSet;
//import java.util.HashMap;
import java.util.List;
//import java.util.Map;
//import java.util.Properties;
import java.util.Set;
import org.appdapter.core.matdat.EnhancedRepoClient;
import org.appdapter.core.name.Ident;
//import org.jflux.api.registry.Registry;
//import org.jflux.impl.registry.OSGiRegistry;
import org.osgi.framework.BundleContext;
//import org.robokind.api.common.lifecycle.ManagedService;
//import org.robokind.api.common.lifecycle.ServiceLifecycleProvider;
//import org.robokind.api.common.lifecycle.utils.SimpleLifecycle;
//import org.robokind.api.common.osgi.lifecycle.OSGiComponent;

/**
 *
 * @author Jason Randolph Eads <jeads362@gmail.com>
 */


public class ServiceManagerTestWiring {
    
//    public static final String GROUP_KEY_FOR_CONNECION_SPEC = "connectionSpecGroupId";
//    public static final String CONNECTION_GROUP_QN = "demoConnectionGroup";
    
//    public static ConnectionSpecExtender startSpecExtender(BundleContext bundleCtx, String optionalSpecFilter) {
//        Registry reg = new OSGiRegistry(bundleCtx);
////        ConnectionSpecExtender cse = new ConnectionSpecExtender(bundleCtx, reg, optionalSpecFilter);
////        cse.start();
////        return cse;
//        return null;
//    }
    
    public static void loadSpecs(
            BundleContext context,
            EnhancedRepoClient defaultDemoRepoClient,
//            String graphQN) {
            String... graphQNs) {
    
    
    
//    public static void loadSpecs(
//            BundleContext context,
//            EnhancedRepoClient defaultDemoRepoClient,
//            String pipelineGraphQN,
//            String mergedModelGraphQN) {
//        Set<Object> assembledRootOfPipelines = assembleRoots(defaultDemoRepoClient,pipelineGraphQN);
//        Set<Object> assembledRoots = assembleRoots(defaultDemoRepoClient,mergedModelGraphQN);
        
        
        
//        Set<Object> assembledRoots = assembleRoots(defaultDemoRepoClient,graphQN);
//        
        
        Set<Object> assembledRoots = new  HashSet<Object>();
        // Construct RDF objects for each graph
        for( String graphQN : graphQNs ) {
            assembledRoots.addAll(assembleRoots(defaultDemoRepoClient,graphQN));
        }
        
        List<PropertySpec> props =
                loadPropertySpecs(assembledRoots);
        
        List<DefaultRegistrationStrategySpec> regStrats =
                loadDefaultRegistrationStrategySpecs(assembledRoots);
        
        List<ServiceBindingSpec> servBindings =
                loadServiceBindingSpecs(assembledRoots);
        
        List<ServiceManagerSpec> servManagers = 
        loadServiceManagerSpecs(assembledRoots);
    }
    
    /**
     * Collects the data from the repo, building it into untyped spec objects
     * 
     * @param defaultDemoRepoClient
     * @param graphQN
     * @return 
     */
    private static Set<Object> assembleRoots(
            EnhancedRepoClient defaultDemoRepoClient, String graphQN) {
        // Determine the URI for the 'qualified name' which identifies the data 
        // in the repo
        Ident graphID = defaultDemoRepoClient.makeIdentForQName(graphQN);

        // Collect the objects from the repo, building them from RDF raw data
        return defaultDemoRepoClient.assembleRootsFromNamedModel(graphID);
    }
    
    /**
     * Extracts typed PropertySpecs from the assembled data objects
     * 
     * @param assembledRoots
     * @return 
     */
    private static List<PropertySpec> loadPropertySpecs(
            Set<Object> assembledRoots) {
        List<PropertySpec> specs = new ArrayList();
        for (Object root : assembledRoots) {
            // Ignore anything that is not a PropertySpec
            if (root == null || 
                    !PropertySpec.class.isAssignableFrom(root.getClass())) {
                continue;
            }
            specs.add((PropertySpec) root);
        }
        return specs;
    }
    
    private static List<ServiceBindingSpec> loadServiceBindingSpecs(
            Set<Object> assembledRoots) {
        List<ServiceBindingSpec> specs = new ArrayList();
        for (Object root : assembledRoots) {
            // Ignore anything that is not a PropertySpec
            if (root == null || 
                    !ServiceBindingSpec.class.isAssignableFrom(root.getClass())) {
                continue;
            }
            specs.add((ServiceBindingSpec) root);
        }
        return specs;
    }
    
    private static List<DefaultRegistrationStrategySpec> loadDefaultRegistrationStrategySpecs(
            Set<Object> assembledRoots) {
        List<DefaultRegistrationStrategySpec> specs = new ArrayList();
        for (Object root : assembledRoots) {
            // Ignore anything that is not a PropertySpec
            if (root == null || 
                    !DefaultRegistrationStrategySpec.class.isAssignableFrom(root.getClass())) {
                continue;
            }
            specs.add((DefaultRegistrationStrategySpec) root);
        }
        return specs;
    }
    
    private static List<ServiceManagerSpec> loadServiceManagerSpecs(
            Set<Object> assembledRoots) {
        List<ServiceManagerSpec> specs = new ArrayList();
        for (Object root : assembledRoots) {
            // Ignore anything that is not a PropertySpec
            if (root == null || 
                    !ServiceManagerSpec.class.isAssignableFrom(root.getClass())) {
                continue;
            }
            specs.add((ServiceManagerSpec) root);
        }
        return specs;
    }
    
//    private static List<ServiceManagerSpec> loadServiceManagerSpecs(
//            Set<Object> assembledRoots) {
//        List<ServiceManagerSpec> specs = new ArrayList();
//        for (Object root : assembledRoots) {
//            // Ignore anything that is not a PropertySpec
//            if (root == null || 
//                    !ServiceManagerSpec.class.isAssignableFrom(root.getClass())) {
//                continue;
//            }
//            specs.add((ServiceManagerSpec) root);
//        }
//        return specs;
//    }
    
//    /**
//     * Allows JFlux to register connection specs
//     * @param context the BundleContext used to register the spec
//     * @param connectionSpec the spec to be registered
//     */
//    private static ManagedService registerConnectionSpec(
//            BundleContext context, ConnectionSpec connectionSpec) {
//        Properties props = new Properties();
//        props.put(GROUP_KEY_FOR_CONNECION_SPEC, CONNECTION_GROUP_QN);
//        
//        ServiceLifecycleProvider lifecycle = 
//                new SimpleLifecycle(connectionSpec, ConnectionSpec.class);
//        ManagedService ms = new OSGiComponent(context, lifecycle, props);
//        ms.start();
//        return ms;
//    }
    
}
