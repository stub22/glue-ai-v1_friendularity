package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;

/**
 *
 * @author Major Jacquote II <mjacquote@gmail.com>
 */
public class BundleContextSpecBuilder extends CachingComponentAssembler<BundleContextSpec> {
    private final static String id="http://www.friedularity.org/Connection#libraryID";
            
    public BundleContextSpecBuilder() {
    }

    @Override
    protected Class<BundleContextSpec> decideComponentClass(Ident ident, Item item) {
        return BundleContextSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            BundleContextSpec bundleContextSpec,
            Item item,
            Assembler asmblr,
            Mode mode) {

        
    }
}