package org.friendularity.spec.connection;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;
import com.hp.hpl.jena.rdf.model.Resource;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 * @author Jason R. Eads <jeads362@gmail.com>
 */

public class DestinationSpecBuilder
    extends CachingComponentAssembler<DestinationSpec> {
    private final static String theDestinationName = "destinationName";
    private final static String theNodeType = "http://www.friedularity.org/Connection#nodeType";
    private final static String prefix="http://www.friedularity.org/Connection#";
    
    public DestinationSpecBuilder( Resource builderConfRes ) {
        super(builderConfRes);
    }
    
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

        mkc.setType(prefix+reader.readConfigValString(item.getIdent(), theNodeType, item,""));
//        Ident nodePropId = ItemFuncs.getNeighborIdent(
//                item,theNodeType);
//        Item nodeTypeItem = item.getSingleLinkedItem(nodePropId, Item.LinkDirection.FORWARD);
     //   mkc.setType(nodeTypeItem.getIdent().getAbsUriString());
    }
}
