
package org.friendularity.demo.connection;

import java.util.List;
import org.cogchar.outer.behav.demo.AnimationConnector;
import org.cogchar.outer.behav.demo.RepoConnector;
import org.cogchar.outer.behav.demo.RobotConnector;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

// The new two promiscuous imports allows running of both cogchar-1.0.6.2 and cogchar-1.0.7.0
import org.cogchar.blob.emit.*;
import org.appdapter.core.matdat.*;
import org.cogchar.platform.util.ClassLoaderUtils;

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
	public final static String ROBOT_CONNECTION_ENV_VAR_KEY = "com.hrkind.robot.connections";
    
    /**
     * 
     * 
     * @param context metadata for the execution context
     * @throws Exception 
     */
    public void start(BundleContext context) throws Exception {
//		try {
//			AnimationConnector.launchPortableAnimEventFactory(context);
//			RobotConnector.connectRobotsFromSysEnv(context, ROBOT_CONNECTION_ENV_VAR_KEY);
//		} catch (Throwable t) {
//			System.err.print("Connection Problem:\n" + t.toString());
//		}
        
        // Setup to connect to a GoogSheet repo
        List<ClassLoader> classloaders = ClassLoaderUtils.getFileResourceClassLoaders(context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        OnlineSheetRepoSpec repoSpec = new OnlineSheetRepoSpec("0AsAJ7pzOB_F2dGlOTmxWVlQ2cVhRR1RjdE53cjF5VkE", 1, 2, classloaders);
        
//        
        RepoConnector rc = new RepoConnector();
        EnhancedRepoClient enhancedRepoSpec = rc.connectDemoRepoClient(repoSpec);
        
//        RepoConnector repoConn = new RepoConnector();
//		EnhancedRepoClient defaultDemoRepoClient = repoConn.makeRepoClientForDefaultOnlineSheet(context);
        
        ConnectionWiringDemo connectionWiringDemo = new ConnectionWiringDemo(context, defaultDemoRepoClient);
        
        // Allows the wiring demo to pull in any needed dependancies
        connectionWiringDemo.registerJFluxExtenders(context);
        
        // Pull connectionSpecs from repo and publish to JFlux
        connectionWiringDemo.initialConnectionLoad(context, defaultDemoRepoClient, CONNECTION_GRAPH_QN);
        
        // Start the services Panel to view the contents of the object registry.
        startServicesPanel(context);

        
    }
    
    public void startServicesPanel(final BundleContext context) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override public void run() {
                ServicesFrame sf = new ServicesFrame();
                sf.setBundleContext(context);
                sf.setVisible(true);
            }
        });
    }
    
    
    /**
     * 
     * 
     * @param context metadata for the execution context
     * @throws Exception 
     */
    public void stop(BundleContext context) throws Exception {
        //TODO add deactivation code here
    }
}
