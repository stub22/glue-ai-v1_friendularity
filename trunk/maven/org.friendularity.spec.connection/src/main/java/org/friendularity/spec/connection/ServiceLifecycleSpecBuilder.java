package org.friendularity.spec.connection;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class ServiceLifecycleSpecBuilder
    extends CachingComponentAssembler<ServiceLifecycleSpec> {
    private final static String theLifecycleJavaFQCN = "lifecycleJavaFQCN";
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
        ClassLoader loader = ClassLoader.getSystemClassLoader();
        try {
            mkc.setLifecycleClass(loader.loadClass(className));
        } catch(ClassNotFoundException e) {
            theLogger.log(Level.SEVERE, "Class {0} not found.", className);
            mkc.setLifecycleClass(null);
        }
    }
    
}
