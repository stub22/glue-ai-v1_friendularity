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
import org.jflux.api.service.DefaultRegistrationStrategy;
import org.jflux.api.service.RegistrationStrategy;
import org.jflux.api.service.binding.ServiceBinding;
import com.hp.hpl.jena.rdf.model.Resource;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 * @author Jason R. Eads <jeads362@gmail.com>
 */

public class ServiceManagerSpecBuilder
    extends CachingComponentAssembler<ServiceManagerSpec> {
    private final static String theLifecycleType = 
            "http://www.cogchar.org/schema/scene#lifecycleType";
    private final static String theServiceBinding = 
            "http://www.cogchar.org/schema/scene#serviceBinding";
    private final static String theRegistrationStrategy =
            "http://www.cogchar.org/schema/scene#registrationStrategy";
    private final static Logger theLogger =
            Logger.getLogger(ServiceManagerSpecBuilder.class.getName());
    
    
    public ServiceManagerSpecBuilder( Resource builderConfRes ) {
        super(builderConfRes);
    }
    
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
                String lifecycleClass = lifecycleSpec.getLifecycleClassName();
                mkc.setLifecycleClassName(lifecycleClass);
                break;
            } else {
                theLogger.log(
                        Level.WARNING, "Unexpected object found at {0} = {1}",
                        new Object[]{theLifecycleType, lc.toString()});
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
                
                Descriptor desc = bindingSpec.getDescriptor();
                ServiceBinding binding =
                        new ServiceBinding(
                        bindingSpec.getServiceDependency(), desc,
                        bindingSpec.getBindingStrategy());
                mkc.addServiceBinding(desc.getClassName(), binding);
            } else {
                theLogger.log(
                        Level.WARNING, "Unexpected object found at {0} = {1}",
                        new Object[]{theServiceBinding, sb.toString()});
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
            } else {
                theLogger.log(
                        Level.WARNING, "Unexpected object found at {0} = {1}",
                        new Object[]{theRegistrationStrategy, rs.toString()});
            }
        }
    }
}
