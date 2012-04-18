package org.friendularity.bundle.lifter;

//import org.apache.felix.http.api.ExtHttpService;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.http.HttpService;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        // TODO add activation code here
        System.out.println("Hello activated!");
        ServiceReference sRef = context.getServiceReference(HttpService.class.getName());
        if (sRef != null)
        {
            System.out.println("Got service reference");
            HttpService service = (HttpService) context.getService(sRef);
            service.registerServlet("/hello", new HelloWorldServlet(), null, null);
            
            //ExtHttpService service2 = (ExtHttpService) context.getService(sRef);
            //service2.registerFilter(new LiftFilter(), "/lift/.*", null, 0, null);
            
        }
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
