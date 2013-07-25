package org.friendularity.spec.connection;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;
import org.jflux.api.registry.Descriptor;
import org.jflux.api.registry.basic.BasicDescriptor;
import org.jflux.api.service.DefaultRegistrationStrategy;
import org.jflux.api.service.RegistrationStrategy;
import org.jflux.api.service.ServiceLifecycle;
import org.jflux.api.service.binding.ServiceBinding;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class ServiceManagerSpecBuilder
    extends CachingComponentAssembler<ServiceManagerSpec> {
    private final static String theLifecycleType = "lifecycleType";
    private final static String theServiceBinding = "serviceBinding";
    private final static String theRegistrationStrategy =
            "registrationStrategy";
    private final static Logger theLogger =
            Logger.getLogger(ServiceManagerSpecBuilder.class.getName());
    
    @Override
    protected Class<ServiceManagerSpec> decideComponentClass(
            Ident ident, Item item) {
        return ServiceManagerSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            ServiceManagerSpec mkc, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();
        ServiceLifecycleSpec lifecycleSpec = null;
        
        List linkedLifecycles =
                reader.findOrMakeLinkedObjects(
                item, theLifecycleType, asmblr, mode, null);
        List linkedBindings =
                reader.findOrMakeLinkedObjects(
                item, theServiceBinding, asmblr, mode, null);
        List linkedStrategies =
                reader.findOrMakeLinkedObjects(
                item, theRegistrationStrategy, asmblr, mode, null);
        
        for(Object lc: linkedLifecycles) {
            if(lc instanceof ServiceLifecycleSpec) {
                lifecycleSpec = (ServiceLifecycleSpec)lc;
                Class lifecycleClass = lifecycleSpec.getLifecycleClass();
                try {
                    mkc.setLifecycle(
                            (ServiceLifecycle)lifecycleClass.newInstance());
                } catch(Exception e) {
                    theLogger.log(
                            Level.SEVERE, "Error instantiating lifecycle: {0}",
                            e.getMessage());
                    mkc.setLifecycle(null);
                }
                break;
            }
        }
        
        for(Object sb: linkedBindings) {
            if(sb instanceof ServiceBindingSpec) {
                ServiceBindingSpec bindingSpec = (ServiceBindingSpec)sb;
                
                if(lifecycleSpec != null) {
                    bindingSpec.setCardinality(lifecycleSpec.getCardinality());
                    bindingSpec.setUpdateStrategy(
                            lifecycleSpec.getUpdateStrategy());
                } else {
                    theLogger.severe(
                            "No source of cardinality or update strategy.");
                }
                
                ServiceBinding binding = new ServiceBinding(
                        bindingSpec.getServiceDependency(),
                        bindingSpec.getDescriptor(),
                        bindingSpec.getBindingStrategy());
                mkc.addServiceBinding(
                        bindingSpec.getDescriptor().getClassName(), binding);
            }
        }
        
        for(Object rs: linkedStrategies) {
            if(rs instanceof DefaultRegistrationStrategySpec) {
                DefaultRegistrationStrategySpec stratSpec =
                        (DefaultRegistrationStrategySpec)rs;
                RegistrationStrategy strat =
                        new DefaultRegistrationStrategy(
                        stratSpec.getClassNames(),
                        stratSpec.getRegistrationProperties());
                mkc.setServiceRegistration(strat);
                break;
            }
        }
    }
}
