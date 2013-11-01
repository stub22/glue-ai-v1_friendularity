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
 * @author Major Jacquote II <mjavquote@gmail.com>
 */
public class RemoteClientPropertySpecBuilder extends CachingComponentAssembler<RemoteClientPropertySpec> {

    private final String theSpeechServiceId="http://www.friedularity.org/Connection#speechServiceIDRC";
    private final String theRemoteId="http://www.friedularity.org/Connection#remoteIDRC";
    
    public RemoteClientPropertySpecBuilder(Resource builderConfRes) {
        super(builderConfRes);
    }

    @Override
    protected Class<RemoteClientPropertySpec> decideComponentClass(Ident ident, Item item) {
        return RemoteClientPropertySpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(RemoteClientPropertySpec rcps, Item item, Assembler asmblr, Mode mode) {
        
        ItemAssemblyReader reader = getReader();
        String speechServiceID=reader.readConfigValString(item.getIdent(), theSpeechServiceId, item, "");
        String remoteID=reader.readConfigValString(item.getIdent(), theRemoteId, item, "");
        rcps.setSpeechServiceId(speechServiceID);
        rcps.setRemoteId(remoteID);
        
    
    }
}
