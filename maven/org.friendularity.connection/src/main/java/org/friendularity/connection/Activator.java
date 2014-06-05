package org.friendularity.connection;

import java.util.List;

import org.appdapter.core.boot.ClassLoaderUtils;
// import org.cogchar.outer.behav.demo.RepoConnector;
import org.osgi.framework.BundleContext;

// import org.appdapter.core.repo.*;

import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.core.repo.EnhancedRepoClient;
import org.appdapter.core.store.Repo;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.jflux.spec.services.RegisterWiring;

/**
 * Bundle Activator for the demo connection package. This project serves as an example use-case for
 * the glueAI data flow system. Information is pulled from repositories, in this case a Google
 * spreadsheet, and is converted to in-system information.
 *
 * @author Yishuai Li & Jason Eads
 */
public class Activator extends BundleActivatorBase {
    private final static String MERGED_MODEL_MANAGER_QN =
            "csi:merged_manager_1001";
    
    @Override
    public void start(BundleContext context) throws Exception {
        forceLog4jConfig();
		scheduleFrameworkStartEventHandler(context);
    }

    @Override 
    protected void handleFrameworkStartedEvent(BundleContext context) {        
        List<ClassLoader> classloaders =
                ClassLoaderUtils.getFileResourceClassLoaders(
                context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        OnlineSheetRepoSpec repoSpec =
                new OnlineSheetRepoSpec(
                        "0AivIV8RvlFTvdGlqNVlMUkQyd0szb1VMSGtYSDBfNHc",
                        0, 7, classloaders);
        
        String QUERY_SOURCE_GRAPH_QN = "ccrt:qry_sheet_77";
        String TGT_GRAPH_SPARQL_VAR = "qGraph";
        
        Repo.WithDirectory bmcMemoryRepoHandle = repoSpec.makeRepo();
        EnhancedRepoClient enhancedRepoSpec = new EnhancedRepoClient(repoSpec, bmcMemoryRepoHandle,
                TGT_GRAPH_SPARQL_VAR, QUERY_SOURCE_GRAPH_QN);

        RegisterWiring.loadAndRegisterSpec(context, enhancedRepoSpec, MERGED_MODEL_MANAGER_QN);
        RegisterWiring.startSpecExtender(context, null);
    }

    @Override
    public void stop(BundleContext context) throws Exception {
    }
}
