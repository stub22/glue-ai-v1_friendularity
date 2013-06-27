/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.DynamicCachingComponentAssembler;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;

import com.hp.hpl.jena.assembler.assemblers.AssemblerBase;
import com.hp.hpl.jena.rdf.model.Resource;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;


/**
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionSpecBuilder extends DynamicCachingComponentAssembler<ConnectionSpec> {
    
    private final static String   ipAddress = "ipAddress";
    private final static String   port = "port";
    private final static String   username = "username";
    private final static String   password = "password";
    private final static String   clientName = "clientName";
    private final static String   virtualHost = "virtualHost";
    private final static String   connectionOptions = "connectionOptions";
    
    /**
     * 
     * TODO: rename builderConfRes to be more meaningful
     * @param builderConfRes 
     */
    public ConnectionSpecBuilder( Resource builderConfRes ) {
        super(builderConfRes);
    }
    
    @Override
    protected Class<ConnectionSpec> decideComponentClass(Ident ident, Item item) {
        return ConnectionSpec.class;
//        throw new UnsupportedOperationException("Not supported yet.");
    }

    /**
     * This extracts the data from the data source and injects it into a spec
     * object.
     * 
     * @param connectionSpec - the spec that is being populated with data
     * @param item - provides identity of item from data source
     * @param asmblr - NOT USED IN THIS CASE
     * @param mode  - NOT USED IN THIS CASE
     */
    @Override
    protected void initExtendedFieldsAndLinks(ConnectionSpec connectionSpec, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader =  getReader();
        connectionSpec.setIpAddress(reader.readConfigValString(item.getIdent(), ipAddress, item, ""));
        connectionSpec.setPort(reader.readConfigValString(item.getIdent(), port, item, ""));
        connectionSpec.setUsername(reader.readConfigValString(item.getIdent(), username, item, ""));
        connectionSpec.setPassword(reader.readConfigValString(item.getIdent(), password, item, ""));
        connectionSpec.setClientName(reader.readConfigValString(item.getIdent(), clientName, item, ""));
        connectionSpec.setVirtualHost(reader.readConfigValString(item.getIdent(), virtualHost, item, ""));
        connectionSpec.setConnectionOptions(reader.readConfigValString(item.getIdent(), connectionOptions, item, ""));
    }
}
