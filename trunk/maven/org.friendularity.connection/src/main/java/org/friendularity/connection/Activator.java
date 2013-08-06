
package org.friendularity.connection;

import org.friendularity.spec.connection.ConnectionWiring;
import java.util.List;
import org.cogchar.outer.behav.demo.RepoConnector;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
//import org.friendularity.spec.connection.ServiceManagerWiring;

import org.appdapter.core.matdat.*;
import org.cogchar.platform.util.ClassLoaderUtils;
import org.friendularity.spec.connection.ServiceManagerTestWiring;

/**
 * Bundle Activator for the demo connection package. This project serves as an 
 * example use-case for the glueAI data flow system. Information is pulled from
 * repositories, in this case a Google spreadsheet, and is converted to
 * in-system information.
 * 
 * @author Yishuai Li & Jason Eads
 */
public class Activator implements BundleActivator {
    
    private final static String CONNECTION_GRAPH_QN = "fc:connection_sheet_1";
    private final static String SERVICE_BINDING_GRAPH_QN = "csi:service_manager_1";
    private final static String LIFECYCLE_DEFINITION_GRAPH_QN = "csi:lifecycle_1";
    private final static String PIPELINE_QN = "csi:pipeline_sheet_77";
    private final static String MERGED_MODEL_MANAGER_QN = "csi:merged_manager_1001";
    
    
//    private final static String CONNECTION_GRAPH_QN = "http://www.friedularity.org/Connection#connection_sheet_1";
//    private final static String SERVICE_BINDING_GRAPH_QN = "http://www.cogchar.org/schema/scene/instance#service_manager_1";
//    private final static String LIFECYCLE_DEFINITION_GRAPH_QN = "http://www.cogchar.org/schema/scene/instance#lifecycle_1";
    
//    private final static String SERVICE_MANAGER_MERGED_MODEL_GRAPH_QN = "csi:merged_manager_1001";
    
    public void start(BundleContext context) throws Exception {
        // Setup to connect to a GoogSheet repo
        List<ClassLoader> classloaders = 
                ClassLoaderUtils.getFileResourceClassLoaders(context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        OnlineSheetRepoSpec repoSpec = 
                //new OnlineSheetRepoSpec("0AlpQRNQ-L8QUdFh5YWswSzdYZFJMb1N6aEhJVWwtR3c", 4, 3, classloaders);
                new OnlineSheetRepoSpec("0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc", 9, 8, classloaders);
        RepoConnector rc = new RepoConnector();
        EnhancedRepoClient enhancedRepoSpec = rc.connectDemoRepoClient(repoSpec);

//        ServiceManagerTestWiring.loadSpecs(
//                context,
//                enhancedRepoSpec,
//                CONNECTION_GRAPH_QN);
        
//        ServiceManagerTestWiring.loadSpecs(context, enhancedRepoSpec, PIPELINE_QN, MERGED_MODEL_MANAGER_QN);        
        ServiceManagerTestWiring.loadSpecs(context, enhancedRepoSpec, SERVICE_BINDING_GRAPH_QN, LIFECYCLE_DEFINITION_GRAPH_QN);
//        ServiceManagerTestWiring.loadSpecs(context, enhancedRepoSpec, SERVICE_BINDING_GRAPH_QN, LIFECYCLE_DEFINITION_GRAPH_QN, CONNECTION_GRAPH_QN);
//        ServiceManagerTestWiring.loadServiceBindingSpecs(context, enhancedRepoSpec, SERVICE_BINDING_GRAPH_QN, LIFECYCLE_DEFINITION_GRAPH_QN);
//        ServiceManagerTestWiring.loadServiceBindingSpecs(context, enhancedRepoSpec, SERVICE_MANAGER_MERGED_MODEL_GRAPH_QN);
        
        
        //Extender listens for specs and creates lfecycles
//        ConnectionWiring.startSpecExtender(context, null);
        //ServiceManagerWiring$.MOUDLE$.loadAndRegisterSpecs(context, enhancedRepoSpec, SERVICE_BINDING_GRAPH_QN);
        //ServiceManagerWiring$ c;
        //$.meep();
//        ServiceManagerWiring x = new ServiceManagerWiring();
//        x.loadAndRegisterSpecs(context, enhancedRepoSpec, SERVICE_BINDING_GRAPH_QN);
//        
        
        // Pull connectionSpecs from repo and publish to JFlux
//        ConnectionWiring.loadAndRegisterSpecs(context, enhancedRepoSpec, CONNECTION_GRAPH_QN);   
    }
    
    @Override
    public void stop(BundleContext context) throws Exception {}
}
