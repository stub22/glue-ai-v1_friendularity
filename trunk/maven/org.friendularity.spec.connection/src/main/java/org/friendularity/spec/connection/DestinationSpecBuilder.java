package org.friendularity.spec.connection;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import java.util.Set;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.item.ItemFuncs;
import org.appdapter.core.name.Ident;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class DestinationSpecBuilder
    extends CachingComponentAssembler<DestinationSpec> {
    private final static String theDestinationName = "destinationName";
    private final static String theNodeType = "nodeType";
    
    @Override
    protected Class<DestinationSpec> decideComponentClass(
            Ident ident, Item item) {
        return DestinationSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            DestinationSpec mkc, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();
        mkc.setName(reader.readConfigValString(
                item.getIdent(), theDestinationName, item, ""));
        
        Ident nodePropId = ItemFuncs.getNeighborIdent(
                item, "http://www.friedularity.org/Connection#" + theNodeType);
        Item nodeTypeItem = item.getSingleLinkedItem(nodePropId, Item.LinkDirection.FORWARD);
        mkc.setType(nodeTypeItem.getIdent().getLocalName());
    }
}
