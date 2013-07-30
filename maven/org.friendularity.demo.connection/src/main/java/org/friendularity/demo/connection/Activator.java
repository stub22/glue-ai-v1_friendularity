
package org.friendularity.demo.connection;

import org.friendularity.spec.connection.ConnectionWiring;
import java.util.List;
import org.cogchar.outer.behav.demo.RepoConnector;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import org.appdapter.core.matdat.*;
import org.cogchar.platform.util.ClassLoaderUtils;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

/**
 * Bundle Activator for the demo connection package. This project serves as an 
 * example use-case for the glueAI data flow system. Information is pulled from
 * repositories, in this case a Google spreadsheet, and is converted to
 * in-system information.
 * 
 * @author Yishuai Li & Jason Eads
 */
public class Activator implements BundleActivator {
    
    private final static String CONNECTION_GRAPH_QN = 
            "fc:connection_sheet_example";
    
    public void start(BundleContext context) throws Exception {
        // Setup to connect to a GoogSheet repo
        List<ClassLoader> classloaders =
                ClassLoaderUtils.getFileResourceClassLoaders(
                context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        OnlineSheetRepoSpec repoSpec =
                new OnlineSheetRepoSpec(
                "0AsAJ7pzOB_F2dGlOTmxWVlQ2cVhRR1RjdE53cjF5VkE", 1, 2,
                classloaders);
        RepoConnector rc = new RepoConnector();
        EnhancedRepoClient enhancedRepoSpec =
                rc.connectDemoRepoClient(repoSpec);
        
        //Extender listens for specs and creates lifecycles
        ConnectionWiring.startSpecExtender(context, null);
        
        // Pull connectionSpecs from repo and publish to JFlux
        ConnectionWiring.loadAndRegisterSpecs(
                context, enhancedRepoSpec, CONNECTION_GRAPH_QN);
        
        // Display the JFlux services
        startServicePanel(context);
    }
    
    private void startServicePanel(final BundleContext context) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                ServicesFrame sf = new ServicesFrame();
                sf.setBundleContext(context);
                sf.setVisible(true);
            }
        });
    }
    
    @Override
    public void stop(BundleContext context) throws Exception {}
}
