package org.friendularity.spec.connection;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.appdapter.core.component.KnownComponentImpl;
import org.appdapter.core.matdat.BoundModelProvider;
import org.appdapter.core.matdat.EnhancedRepoClient;
import org.appdapter.core.matdat.ModelProviderFactory;
import org.appdapter.core.matdat.PipelineQuerySpec;
import org.appdapter.core.name.Ident;
import org.jflux.api.registry.Registry;
import org.jflux.impl.registry.OSGiRegistry;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.lifecycle.ManagedService;
import org.robokind.api.common.lifecycle.ServiceLifecycleProvider;
import org.robokind.api.common.lifecycle.utils.SimpleLifecycle;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;

/**
 *
 * @author robokind
 */
public class RegisterWiring {

    public final static String PIPELINE_GRAPH_QN = "csi:pipeline_sheet_0";
    public final static String PIPE_QUERY_QN = "ccrt:find_pipes_77"; //"ccrt:find_sheets_77";
    public final static String PIPE_SOURCE_QUERY_QN = "ccrt:find_pipe_sources_99";
    public final static PipelineQuerySpec myDefaultPipelineQuerySpec = new PipelineQuerySpec(
            PIPE_QUERY_QN, PIPE_SOURCE_QUERY_QN, PIPELINE_GRAPH_QN);

    public static List<ManagedService> loadAndRegisterSpec(BundleContext context, EnhancedRepoClient defaultDemoRepoClient, String mergedSheetQN) {
        Ident derivedBehavGraphID = defaultDemoRepoClient.makeIdentForQName(mergedSheetQN);
        List<ManagedService> managedServices = new ArrayList();
        BoundModelProvider derivedBMP =
                ModelProviderFactory.makeOneDerivedModelProvider(
                defaultDemoRepoClient, myDefaultPipelineQuerySpec, derivedBehavGraphID);

        Set<Object> allSpecs = derivedBMP.assembleModelRoots();

        List<RegistrationSpec> registrationSpecs = filterSpecs(RegistrationSpec.class, allSpecs);

        for (RegistrationSpec root : registrationSpecs) {
            if (root instanceof RegistrationSpec) {
                ManagedService<RegistrationSpec> rs =
                        registerSpec(context, root);
                managedServices.add(rs);
            }
        }
        return managedServices;

    }

    private static <T> List<T> filterSpecs(Class<T> classes, Set<Object> rawSpecs) {
        List<T> specs = new ArrayList();
        for (Object root : rawSpecs) {
            // Ignore anything that is not of type T
            if (root == null || !classes.isAssignableFrom(root.getClass())) {
                continue;
            }
            specs.add((T) root);
        }
        return specs;
    }

    private static ManagedService<RegistrationSpec> registerSpec(BundleContext context, RegistrationSpec spec) {

        ServiceLifecycleProvider<RegistrationSpec> lifecycle =
                new SimpleLifecycle<RegistrationSpec>(spec, RegistrationSpec.class);

        ManagedService<RegistrationSpec> ms = new OSGiComponent<RegistrationSpec>(context, lifecycle, spec.getProperties());
        ms.start();
        return ms;
    }
}
