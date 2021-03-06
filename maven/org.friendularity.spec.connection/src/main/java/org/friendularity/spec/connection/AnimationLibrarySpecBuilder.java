package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;

import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;

public class AnimationLibrarySpecBuilder extends CachingComponentAssembler<AnimationLibrarySpec> {
    private final static String id="http://www.friedularity.org/Connection#libraryID";
            
    public AnimationLibrarySpecBuilder() {
    }

    @Override
    protected Class<AnimationLibrarySpec> decideComponentClass(Ident ident, Item item) {
        return AnimationLibrarySpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            AnimationLibrarySpec animationLibrarySpec,
            Item item,
            Assembler asmblr,
            Mode mode) {

        ItemAssemblyReader reader = getReader();
        animationLibrarySpec.setId(reader.readConfigValString(item.getIdent(), id, item, null));
    }
}
