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
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.appdapter.core.name.Ident;
import org.osgi.framework.BundleContext;
import org.appdapter.core.matdat.*;
import org.robokind.api.common.lifecycle.utils.SimpleLifecycle;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;

/**
 *
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionWiring {
	//private ManagedServiceFactory	myManagedServiceFactory;
	private EnhancedRepoClient      myRepoClient;
    private BundleContext           myBundleContext;

    public static final String GROUP_KEY_FOR_CONNECION_SPEC = "connectionSpecGroupID";
    public static final String CONNECTION_GROUP_QN = "UNIMPLEMENTED_FEATURE.";

    //TODO: fix the
    /**
     * Manages the process of wiring the Repo raw data into usable JFlux Specs.
     * 
     * @param aManagedServiceFactory ...
     * @param aBundleContext the bundle Context for the current OSGi/JFlux instance
     * @param aConnectedRepoClient Repo Client that pulls data from its repo.
     */
    public ConnectionWiring(BundleContext aBundleContext, EnhancedRepoClient aConnectedRepoClient) {
        myBundleContext = aBundleContext;
        myRepoClient = aConnectedRepoClient;
    }
//    public ConnectionWiring(ManagedServiceFactory aManagedServiceFactory, EnhancedRepoClient aConnectedRepoClient) {
//        myManagedServiceFactory = aManagedServiceFactory;
//        myRepoClient = aConnectedRepoClient;
//    }

    public void registerJFluxExtenders(BundleContext bundleCtx) {
        ConnectionSpecExtender connectionSpecExtender = new ConnectionSpecExtender(bundleCtx, null);
        connectionSpecExtender.start();
    }
    
    void initialConnectionLoad(BundleContext context, EnhancedRepoClient defaultDemoRepoClient, String connectionGraphQN) {
        List<ConnectionSpec> specs = loadConnectionSpecs(defaultDemoRepoClient, connectionGraphQN);
        registerConnectionSpecs(specs, context);
    }

    // TODO: comment
    private List<ConnectionSpec> loadConnectionSpecs(EnhancedRepoClient defaultDemoRepoClient, String connectionGraphQN) {
        List<ConnectionSpec> specs = new ArrayList();

        // Determine the URI for the 'qualified name' which identifies the data in the repo
        Ident connectionGraphID = defaultDemoRepoClient.makeIdentForQName(connectionGraphQN);

        // Collect the specs from the repo, building them from RDF raw data
        Set<Object> allConnectionSpecs = defaultDemoRepoClient.assembleRootsFromNamedModel(connectionGraphID);

        // TODO: implement the filter here? or not?

        // Publish the new spec objects to JFlux
        for (Object spec : allConnectionSpecs) {
            // TODO: CHECK THIS TYPE CHECKING LOGIC PLZ
            // Check for strong typing
            if (spec == null || spec.getClass() != ConnectionSpec.class) {
                continue;
            }
            ConnectionSpec connectionSpec = (ConnectionSpec) spec;
            specs.add(connectionSpec);
        }

        return specs;
    }

    /**
     * Allows JFlux to register connection specs
     * @param specs the specs to be registered.
     * @param context the bundle Context for the current OSGi/JFlux instance
     */
    private void registerConnectionSpecs(List<ConnectionSpec> specs, BundleContext context) {
        for (ConnectionSpec connectionSpec : specs) {
            //TODO: Resolve ManagedServiceFactory issue
            Properties props = null;
            if (GROUP_KEY_FOR_CONNECION_SPEC != null) {
                props = new Properties();
                props.put(GROUP_KEY_FOR_CONNECION_SPEC, CONNECTION_GROUP_QN);
            }
            new OSGiComponent(context, new SimpleLifecycle(connectionSpec, ConnectionSpec.class, props)).start();
        }
    }
}