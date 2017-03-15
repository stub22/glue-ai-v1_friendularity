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

package org.friendularity.anim.loader;

import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import com.hp.hpl.jena.rdf.model.Resource;

import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;

/**
 *
 * @author Jason R. Eads <eadsjr>
 */
public class AnimSpecBuilder extends CachingComponentAssembler<AnimSpec> {

    /**
     *
     * TODO: rename builderConfRes to be more meaningful
     *
     * @param builderConfRes
     */
    AnimSpecBuilder(Resource builderConfRes) {
        super(builderConfRes);
    }
    
    /**
     * These are the names to be used in retrieving a ConnectionSpec from the
     * data source in Appdapter
     */
    private final String PATH_Type = "type";
    private final String PATH_Name = "name";
    private final String PATH_Path = "path";
    private final String PATH_Folder = "folder";

    @Override
    protected Class<AnimSpec> decideComponentClass(Ident componentID, Item componentConfigItem) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    /**
     * This extracts the data from the data source and injects it into a spec
     * object.
     *
     * @param connectionSpec - the spec that is being populated with data
     * @param item - provides identity of item from data source
     * @param asmblr - ...appears to do nothing?
     * @param mode - ...appears to do nothing?
     */
    @Override
    protected void initExtendedFieldsAndLinks(AnimSpec animSpec, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();
        animSpec.setType(reader.readConfigValString(item.getIdent(), PATH_Type, item, ""));
        animSpec.setName(reader.readConfigValString(item.getIdent(), PATH_Name, item, ""));
        animSpec.setPath(reader.readConfigValString(item.getIdent(), PATH_Path, item, ""));
        animSpec.setFolder(reader.readConfigValString(item.getIdent(), PATH_Folder, item, ""));
    }
}
