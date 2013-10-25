 /*
 * Copyright 2013 The Friendularity Project (www.friendularity.org).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;
import org.robokind.impl.messaging.JMSAvroMessageSender;


import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import com.hp.hpl.jena.rdf.model.Resource;

import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
/**
 *
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 */
public class JMSAvroMessageSenderSpecBuilder extends CachingComponentAssembler<JMSAvroMessageSenderSpec> {

    private final static String   theSessionId = "session";
    private final static String   theDestinationId = "destination";
    
    public JMSAvroMessageSenderSpecBuilder ( Resource builderConfRes ) {
        super(builderConfRes);
    }
    
    @Override
    protected Class<JMSAvroMessageSenderSpec> decideComponentClass(Ident ident, Item item) {
        return JMSAvroMessageSenderSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(JMSAvroMessageSenderSpec jmsAvroMessageSenderSpec, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader  = getReader();
        jmsAvroMessageSenderSpec.setSession(reader.readConfigValString(item.getIdent(), theSessionId, item, ""));
        jmsAvroMessageSenderSpec.setDestination(reader.readConfigValString(item.getIdent(), theDestinationId, item, ""));
    }
}
