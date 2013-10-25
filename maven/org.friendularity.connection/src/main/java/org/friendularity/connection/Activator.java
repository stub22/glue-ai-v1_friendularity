package org.friendularity.connection;

import java.util.List;

import javax.swing.UIManager;
// import org.cogchar.outer.behav.demo.RepoConnector;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

// import org.appdapter.core.matdat.*;

import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.core.matdat.EnhancedRepoClient;
import org.appdapter.core.store.Repo;
import org.cogchar.impl.scene.read.BehavMasterConfigTest;

import org.friendularity.spec.connection.RegisterWiring;
import org.cogchar.platform.util.ClassLoaderUtils;
import org.friendularity.spec.connection.ServiceManagerWiring;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

/**
 * Bundle Activator for the demo connection package. This project serves as an example use-case for
 * the glueAI data flow system. Information is pulled from repositories, in this case a Google
 * spreadsheet, and is converted to in-system information.
 *
 * @author Yishuai Li & Jason Eads
 */
public class Activator implements BundleActivator {

//    private final static String CONNECTION_GRAPH_QN =
//            "fc:connection_sheet_1";
    private final static String SERVICE_BINDING_GRAPH_QN =
            "csi:service_manager_1";
    private final static String LIFECYCLE_DEFINITION_GRAPH_QN =
            "csi:lifecycle_1";
//    private final static String PIPELINE_QN = "csi:pipeline_sheet_77";
    private final static String MERGED_MODEL_MANAGER_QN =
            "csi:merged_manager_1001";
    private final static String MERGED_MODEL_MANAGER_QN2 =
            "csi:merged_manager_1002";

    public void start(BundleContext context) throws Exception {
        setLookAndFeel();
        startServicePanel(context);
        // Setup to connect to a GoogSheet repo
        List<ClassLoader> classloaders =
                ClassLoaderUtils.getFileResourceClassLoaders(
                context,
                ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        OnlineSheetRepoSpec repoSpec =
                //                //GluePuma_BehavMasterDemo
                //                new OnlineSheetRepoSpec(
                //                    "0AlpQRNQ-L8QUdFh5YWswSzdYZFJMb1N6aEhJVWwtR3c",
                //                    4,
                //                    3,
                //                    classloaders);
                // GluePuma_HRKR50_TESTFULL
                //                new OnlineSheetRepoSpec(
                //                    "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc",
                //                    9,
                //                    8,
                //                    classloaders);

                new OnlineSheetRepoSpec(
                "0AivIV8RvlFTvdGdCZkQ5Q1k1ZnkwMzg4UkZqbmZfa0E",
                1,
                12,
                classloaders);
        
        Repo.WithDirectory bmcMemoryRepoHandle = repoSpec.makeRepo();
        EnhancedRepoClient enhancedRepoSpec = new EnhancedRepoClient(repoSpec, bmcMemoryRepoHandle,
                BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR(), BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN());

        //RepoConnector rc = new RepoConnector();
        //EnhancedRepoClient enhancedRepoSpec =  rc.connectDemoRepoClient(repoSpec);

        RegisterWiring.loadAndRegisterSpec(context, enhancedRepoSpec, MERGED_MODEL_MANAGER_QN);


        // Load the specs from the repo, and register them with JFlux
        ServiceManagerWiring.loadAndRegisterSpecs(
                context,
                enhancedRepoSpec,
                LIFECYCLE_DEFINITION_GRAPH_QN,
                SERVICE_BINDING_GRAPH_QN,
                MERGED_MODEL_MANAGER_QN2);

        //Extender listens for specs and creates objects from them
        ServiceManagerWiring.startSpecExtender(context, null);
    }

    private void setLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
        }
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
    public void stop(BundleContext context) throws Exception {
    }
}
