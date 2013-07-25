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

/**
 * The builder that is called to make a Spec object from the raw RDF data
 * representing a ServiceBinding.
 * 
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class ServiceBindingSpecBuilder
    extends CachingComponentAssembler<ServiceBindingSpec> {
    // Defines the relationship "#Property Name" key (aka the Predicate),
    // that is followed from an individual to collect the data
    
    private final static String theServiceJavaFQCN = "serviceJavaFQCN";
    private final static String theHasProperty = "hasProperty";
    private final static Logger theLogger =
            Logger.getLogger(ServiceBindingSpecBuilder.class.getName());

    @Override
    protected Class<ServiceBindingSpec> decideComponentClass(
            Ident ident, Item item) {
        return ServiceBindingSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            ServiceBindingSpec mkc, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();
        
        // read in the data field and store it in the Spec
        mkc.setClassName(reader.readConfigValString(
                item.getIdent(), theServiceJavaFQCN, item, ""));
        
        // read in and build the linked properties, and storing each in the Spec
        List linkedProperties = reader.findOrMakeLinkedObjects(
                item, theHasProperty, asmblr, mode, null);
        
        for(Object o: linkedProperties) {
            if(o instanceof PropertySpec) {
                PropertySpec propertySpec = (PropertySpec)o;
                mkc.addProperty(
                        propertySpec.getName(), propertySpec.getValue());
            } else {
                theLogger.log(
                        Level.WARNING,
                        "Unexpected object found at " + theHasProperty +
                        " = {0}", o.toString());
            }
        }
    }
    
}