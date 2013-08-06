package org.friendularity.spec.connection;

import java.util.HashMap;
import java.util.Map;
import org.appdapter.core.component.KnownComponentImpl;
import org.jflux.api.service.RegistrationStrategy;
import org.jflux.api.service.ServiceLifecycle;
import org.jflux.api.service.ServiceManager;
import org.jflux.api.service.binding.ServiceBinding;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class ServiceManagerSpec extends KnownComponentImpl {
    private String myLifecycleClassName;
    private Map<String, ServiceBinding> myServiceBindings;
    private RegistrationStrategy myServiceRegistration;
    private RegistrationStrategy<ServiceManager> myManagerRegistration;
    
    public ServiceManagerSpec() {
        myServiceBindings = new HashMap<String, ServiceBinding>();
        myManagerRegistration = null;
    }
    
    public String getLifecycleClassName() {
        return myLifecycleClassName;
    }
    
    public Map<String, ServiceBinding> getServiceBindings() {
        return myServiceBindings;
    }
    
    public RegistrationStrategy getServiceRegistration() {
        return myServiceRegistration;
    }
    
    public RegistrationStrategy<ServiceManager> getManagerRegistration() {
        return myManagerRegistration;
    }
    
    public void setLifecycleClassName(String lifecycleClassName) {
        myLifecycleClassName = lifecycleClassName;
    }
    
    public void addServiceBinding(String name, ServiceBinding binding) {
        myServiceBindings.put(name, binding);
    }
    
    public void removeServiceBinding(String name) {
        myServiceBindings.remove(name);
    }
    
    public void setServiceRegistration(RegistrationStrategy registration) {
        myServiceRegistration = registration;
    }
    
    public void setManagerRegistration(
            RegistrationStrategy<ServiceManager> registration) {
        myManagerRegistration = registration;
    }
}
