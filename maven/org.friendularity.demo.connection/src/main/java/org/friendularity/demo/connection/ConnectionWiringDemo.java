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
package org.friendularity.demo.connection;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.appdapter.core.name.Ident;
import org.cogchar.outer.behav.demo.WiringDemo;
import org.osgi.framework.BundleContext;
import org.friendularity.spec.connection.ConnectionSpec;

// The new two promiscuous imports allows running of both cogchar-1.0.6.2 and cogchar-1.0.7.0
import org.appdapter.core.matdat.*;

/**
 *
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionWiringDemo extends WiringDemo {

    public static final String GROUP_KEY_FOR_CONNECION_SPEC = "connectionSpecGroupID";
    public static final String CONNECTION_GROUP_QN = "UNIMPLEMENTED_FEATURE.";

    public ConnectionWiringDemo(BundleContext bc, EnhancedRepoClient rc) {
        super(bc, rc);
    }

    @Override
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

    // TODO: comment
    // To JFlux
    private void registerConnectionSpecs(List<ConnectionSpec> specs, BundleContext context) {
        for (ConnectionSpec connectionSpec : specs) {
            Runnable connectionSpecRegistrationRunnable = getRegistrationRunnable(
                    context,
                    ConnectionSpec.class,
                    connectionSpec,
                    GROUP_KEY_FOR_CONNECION_SPEC,
                    CONNECTION_GROUP_QN);

            connectionSpecRegistrationRunnable.run();
        }
    }
}