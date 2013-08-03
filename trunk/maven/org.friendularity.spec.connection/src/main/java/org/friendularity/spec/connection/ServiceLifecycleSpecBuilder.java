package org.friendularity.spec.connection;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;
import org.jflux.api.service.ServiceDependency.Cardinality;
import org.jflux.api.service.ServiceDependency.UpdateStrategy;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class ServiceLifecycleSpecBuilder
    extends CachingComponentAssembler<ServiceLifecycleSpec> {
    private final static String theLifecycleJavaFQCN = "lifecycleJavaFQCN";
    private final static String theUpdateStrategy = "updateStrategy";
    private final static String theCountCardinality = "countCardinality";
    private final static String theRequired = "required";
    private final static Logger theLogger =
            Logger.getLogger(ServiceLifecycleSpecBuilder.class.getName());
    
    @Override
    protected Class<ServiceLifecycleSpec> decideComponentClass(
            Ident ident, Item item) {
        return ServiceLifecycleSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            ServiceLifecycleSpec mkc, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();
        String className = reader.readConfigValString(
                item.getIdent(), theLifecycleJavaFQCN, item, "");
        String update = reader.readConfigValString(
                item.getIdent(), theUpdateStrategy, item, "");
        String count = reader.readConfigValString(
                item.getIdent(), theCountCardinality, item, "");
        String required = reader.readConfigValString(
                item.getIdent(), theRequired, item, "");
        ClassLoader loader = ClassLoader.getSystemClassLoader();
        
        try {
            mkc.setLifecycleClass(loader.loadClass(className));
        } catch(ClassNotFoundException e) {
            theLogger.log(Level.SEVERE, "Class {0} not found.", className);
            mkc.setLifecycleClass(null);
        }
        
        if(update.equals("static")) {
            mkc.setUpdateStrategy(UpdateStrategy.STATIC);
        } else if (update.equals("dynamic")) {
            mkc.setUpdateStrategy(UpdateStrategy.DYNAMIC);
        } else {
            theLogger.log(
                    Level.SEVERE, "Unexpected update strategy: {0}", update);
            mkc.setUpdateStrategy(null);
        }
        
        if(count.equals("single") && required.equals("required")) {
            mkc.setCardinality(Cardinality.MANDATORY_UNARY);
        } else if(count.equals("multiple") && required.equals("required")) {
            mkc.setCardinality(Cardinality.MANDATORY_MULTIPLE);
        } else if(count.equals("single") && required.equals("optional")) {
            mkc.setCardinality(Cardinality.OPTIONAL_UNARY);
        } else if(count.equals("multiple") && required.equals("optional")) {
            mkc.setCardinality(Cardinality.OPTIONAL_MULTIPLE);
        } else {
            theLogger.log(
                    Level.SEVERE, "Unexpected cardinality: {0} {1}",
                    new Object[]{count, required});
            mkc.setCardinality(null);
        }
    }
    
}